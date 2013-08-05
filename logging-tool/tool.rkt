#lang scheme/gui

(require drscheme/tool)
(require "logger.rkt"
         (lib "framework.rkt" "framework")
         "common.rkt"
         "port.rkt"
         "consent.rkt"
         uuid-v4/uuid-v4
         "client.rkt"
         "snip.rkt")

(provide tool@)

(define (frame-mixin frame%)
  (class frame%
    (inherit get-info-panel)
    (super-new)
    
    (define logging-on-message
      (new message%
           [label "logging tool loaded"]
           [font small-control-font]
           [parent (get-info-panel)]))
    
    (define (update-logging-on-message p v)
      (send logging-on-message set-label
            (match v
              ['consented "logging tool on"]
              [else "logging tool off"])))
    
    (update-logging-on-message #f (preferences:get 'logging-tool:consent))
    
    (send logging-on-message show #t)
    
    (preferences:add-callback
     'logging-tool:consent update-logging-on-message)
    
    (preferences:add-callback
     'logging-tool:in-person-name
     (lambda (p v) (tool-set-in-person-name! v)))

    (preferences:add-callback
     'logging-tool:consent
     (lambda (p v) (tool-set-consented! v)))))

(define (tab-mixin tab%)
  (class tab%
    (super-new)
    
    (define tab-identity (make-uuid))
    
    (define logger (new logger%
                        [logger-identity tab-identity]
                        [mute-all-exceptions mute-all-exceptions]))
    
    (send logger log 'new-tab)
    
    (define/public (log event . data)
      (send/apply logger log event data))))

(define (def-text-mixin def-text%)
  (class def-text%
    (super-new)
    
    (define (log event . data) (send/apply (send this get-tab) log event data))
    
    (define (get-save-file)
      (define port (open-output-bytes))
      (send this save-port port)
      (get-output-bytes port))
    
    (define/augment (after-insert start len)
      (with-maybe-silent-failure
       mute-all-exceptions
       (lambda ()
         (log 'insert 'start start 'len len
              'text (get-marshalled-snips-in-range this start len))))
      (inner (void) after-insert start len))
    
    (define/augment (on-delete start len)
      (with-maybe-silent-failure
       mute-all-exceptions
       (lambda () (log 'delete 'start start 'len len
                       'text (get-marshalled-snips-in-range this start len))))
      (inner (void) on-delete start len))
    
    (define/augment (on-load-file filename fmt)
      (with-maybe-silent-failure
       mute-all-exceptions
       (lambda () (log 'load 'filename filename)))
      (inner (void) on-load-file filename fmt))
    
    (define/augment (after-load-file success?)
      (with-maybe-silent-failure
       mute-all-exceptions
       (lambda () (log 'after-load 'save-file (get-save-file))))
      (inner (void) after-load-file success?))
    
    (define/augment (on-save-file filename fmt)
      (with-maybe-silent-failure
       mute-all-exceptions (lambda () (log 'save 'filename filename)))
      (inner (void) on-save-file filename fmt))
    
    (define/augment (after-save-file success?)
      (with-maybe-silent-failure
       mute-all-exceptions
       (lambda () (log 'after-save 'save-file (get-save-file))))
      (inner (void) after-save-file success?))
    
    (define/augment (after-set-position)
      (with-maybe-silent-failure
       mute-all-exceptions
       (lambda () (log 'set-position 'start (send this get-start-position)
                       'end (send this get-end-position)))))
    
    (define/override (do-edit-operation op [recursive? #t] [time 0])
      (with-maybe-silent-failure
       mute-all-exceptions (lambda () (log op)))
      (super do-edit-operation op recursive? time))))

(define tool@
  (unit
    (import drscheme:tool^)
    (export drscheme:tool-exports^)
    
    (define (phase1)
      (add-preference-panel))
    
    (define (phase2)
      (when (eq? (preferences:get 'logging-tool:consent) 'pending)
        (show-consent-dialog))
      (tool-set-in-person-name! (preferences:get 'logging-tool:in-person-name)) ;; do this before turning logging on in the next line.
      (tool-set-consented! (preferences:get 'logging-tool:consent))
      (with-maybe-silent-failure
       mute-all-exceptions
       start-client-daemon))
    
    (drscheme:get/extend:extend-unit-frame frame-mixin)
    
    
    (drscheme:get/extend:extend-tab tab-mixin)
    
    (define drscheme-eventspace (current-eventspace))
    
    ;; Returns all the tabs across all of DrScheme's windows
    (define (get-all-tabs)
      (apply append
             (for/list ([w (parameterize
                               ;; Since the current-event space is the user's eventspace when
                               ;; we are called to log a runtime crash, without the parameterize
                               ;; calling get-top-level-windows would return the user's windows.
                               ([current-eventspace drscheme-eventspace])
                             (get-top-level-windows))]
                        #:when (is-a? w drscheme:unit:frame%))
               (send w get-tabs))))
    
    ;; ---
    
    (define (rep-mixin rep%)
      (class rep%
        (super-new)
        
        (define (get-tab)
          (for/first ([t (get-all-tabs)]
                      #:when (eq? this (send t get-ints)))
            t))
        
        (define/public (log event . data)
          (send/apply (get-tab) log event data))
        
        (define (port-filename port)
          (define v (object-name port))
          (cond [(path? v) (path->bytes v)]
                [(struct? v) #f]
                [else v]))
        
        (define/override (evaluate-from-port port complete-program? cleanup)
          (with-maybe-silent-failure
           mute-all-exceptions
           (lambda ()
             (define-values (p1 p2) (duplicate-port port))
             (set! port p2)
             (let ()
               (define settings (send this get-user-language-settings))
               (define lang (drscheme:language-configuration:language-settings-language settings))
               (define lang-name (send lang get-language-name))
               
               (log (if complete-program? 'execution 'repl)
                    'lang lang-name
                    'filename (port-filename port)
                    'text (save-file-of-port p1)))))
          (super evaluate-from-port port complete-program? cleanup))))
    
    (drscheme:get/extend:extend-interactions-text rep-mixin)
    
    ;; ---
    
    (drscheme:get/extend:extend-definitions-text def-text-mixin)))
