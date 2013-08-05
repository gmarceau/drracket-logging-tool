#lang scheme
(require mzlib/os
         "common.rkt")

(provide logger% tool-set-consented! tool-set-in-person-name!)

(define max-log-file-size-turn-everything-off 100000000)
(define max-log-file-size-turn-code-log-off 90000000)

(define/contract consented
  (box/c (symbols 'consented 'pending 'refused))
  (box 'pending))

(define/contract in-person-name
  (box/c string?)
  (box ""))

(define logger%
  (class object%
    (super-new)
    (init-field logger-identity mute-all-exceptions)
    
    (define/contract log-is-on (box/c (or/c boolean? (symbols 'partially-off)))
      (box #t))
    
    
    (define hostname (gethostname))
    (define username (or (getenv "NWUSERNAME") ;; They have a special system at Adelphi
                         (getenv "USERNAME")
                         (getenv "USER")))
    
    (define (printable? x)
      (or (symbol? x)
          (string? x)
          (bytes? x)
          (boolean? x)
          (char? x)
          (null? x)
          (number? x)
          (regexp? x)
          (prefab-struct-key x) ;; this cannot be last, since it doesn't return just #t
          (pair? x)
          (vector? x)
          (box? x)))
    
    (define (size-hook v like-display? port)
      (if (printable? v) #f 1))
    
    (define (struct->prefab v)
      (apply make-prefab-struct (vector->list (struct->vector v))))
    
    (define (print-hook v like-display? port)
      (define vv
        (cond [(path? v) (make-prefab-struct 'path (system-type 'os) (path->bytes v))]
              [(struct? v) (struct->prefab v)]
              [else
               (let ()
                 (define pp (open-output-bytes))
                 (parameterize ([print-unreadable #t]) (print v pp))
                 (list 'unprintable (get-output-bytes pp)))]))
      
      (parameterize ([pretty-print-columns 'infinity])
        ((if like-display? pretty-display pretty-print) vv port)))
    
    (define (log-file-size)
      (if (not (file-exists? (log-filename)))
          0
          (file-size (log-filename))))
    
    (define (assemble data)
      (match data
        [(list) empty]
        [(list (and (? symbol?) a) b rest ...)
         (cons (list a b) (assemble rest))]))
    
    (define/public (log event-name . data)
      (define (log-now event-name assembled-data)
        (define log-data
          `((event ,event-name)
            ,@assembled-data
            (time ,(seconds->date (current-seconds)))
            (milliseconds ,(current-milliseconds))
            (identity ,logger-identity)
            (hostname ,hostname)
            (in-person-name ,(unbox in-person-name))
            (username ,username)))
        
        (parameterize ([pretty-print-size-hook size-hook]
                       [pretty-print-print-hook print-hook]
                       [print-unreadable #t])
          (call-with-semaphore
           file-semaphore
           (lambda ()
             (with-output-to-file (log-filename) #:exists 'append
               (lambda ()
                 (parameterize ([pretty-print-columns 120])
                   (pretty-print log-data)
                   (newline))))))))
      
      (when (eq? (unbox consented) 'consented)
        (with-maybe-silent-failure
         mute-all-exceptions
         (lambda ()
           (define s (log-file-size))
           
           
           (define (switch-state current desired)
             (set-box! log-is-on desired)
             (log-now 'log-mode-change
                      `((old ,current)
                        (new ,desired)
                        (file-size ,s))))
           
           (define desired-state
             (cond [(> s max-log-file-size-turn-everything-off) #f]
                   [(> s max-log-file-size-turn-code-log-off) 'partially-off]
                   [else #t]))
           
           (define assembled-data (assemble data))
           
           (when (not (eq? (unbox log-is-on) desired-state))
             (switch-state (unbox log-is-on) desired-state))
           
           (when (unbox log-is-on)
             (if (eq? (unbox log-is-on) #t)
                 (log-now event-name assembled-data)
                 ;; When logging is partially-off, remove the 'TEXT from the data
                 (log-now event-name
                          (for/list ([d assembled-data])
                            (match d
                              [(or (list 'text t)
                                   (list 'save-file t))
                               '(full-text-log-turned-off)]
                              [v v])))))))))))


(define (tool-set-consented! v)
  (set-box! consented 'consented)
  (send (new logger% [logger-identity 'none] [mute-all-exceptions #t])
        log 'set-consented 'value v)
  (set-box! consented v))

(define (tool-set-in-person-name! v)
  (send (new logger% [logger-identity 'none] [mute-all-exceptions #t])
        log 'in-person-name-change 'old-value (unbox in-person-name) 'new-value v)
  (set-box! in-person-name v))