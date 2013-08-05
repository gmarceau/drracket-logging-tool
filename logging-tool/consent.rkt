#lang scheme/gui

(require (lib "framework.rkt" "framework"))

(provide add-preference-panel show-consent-dialog)

(define demand-the-student-name true)

(define texts
  '("This installation of DrRacket has been configured to collect"
    "research data on how programmers edit their program while using"
    "DrRacket."
    ""
    "In order to participate in this research, you need to"
    "have received and signed a consent form.  "
    ""
    "The form explains the research project in details "
    "and records your consent to participate."
    ""))

(define (combo->symbol combo)
  (match (send combo get-selection)
    [0 'consented]
    [1 'pending]
    [2 'refused]))

(define (symbol->combo s)
  (match s
    ['consented 0]
    ['pending 1]
    ['refused 2]))

(preferences:set-default 'logging-tool:consent 'pending symbol?)
(preferences:set-default 'logging-tool:in-person-name "" string?)

(define (make-widgets p-frame with-button)
  (define (combo-selection-callback choice . args)
    (define b (and demand-the-student-name (eq? 0 (send choice get-selection))))
    (send txt show b)
    (unless b (send msg show #f)))
  
  (define (button-callback button event)
    (define t (send txt get-value))
    (define c (combo->symbol combo))
    (cond [(and demand-the-student-name
                (eq? 'consented c)
                (or (equal? "" t)
                    (regexp-match #px"^\\s+$" t)))
           (when (not (send msg is-shown?))
             (send msg show #t)
             (sleep 0.1))
           (send msg show #f)
           (sleep 0.1)
           (send msg show #t)]
          [else 
           (on-save-callback)
           (send p-frame show #f)]))
  
  (define (on-save-callback . args)
    (define consented (combo->symbol combo))
    (define name (send txt get-value))
    (preferences:set 'logging-tool:consent consented)
    (preferences:set 'logging-tool:in-person-name name))
  
  (define init-consent (symbol->combo (preferences:get 'logging-tool:consent)))
  (define parent (new vertical-panel% [parent p-frame] [alignment '(left top)]
                      [horiz-margin 30] [border 30]))

  (for ([t texts])
    (new message% [parent parent] [label t]))
  
  (define horizontal-1 (new horizontal-panel% [parent parent] [border 10]))
  (define combo (new choice%
                     [parent horizontal-1]
                     [selection init-consent]
                     [choices '("I have signed the form and consented to participate"
                                "I have not received the form yet (ask this again)"
                                "I would rather not participate (turn recording off)")]
                     [label "Choose one:"]
                     [callback combo-selection-callback]))
  (define horizontal-2 (new horizontal-panel% [parent parent] [border 10]))
  (define txt (new text-field% [label "First and last name:"] [init-value (preferences:get 'logging-tool:in-person-name)] [parent horizontal-2]))
  (define msg (new message% [parent parent] [label "Please enter your name"]))
  
  (send txt show (and demand-the-student-name (eq? 0 init-consent)))
  (send msg show #f)
  (when with-button
    (new button%
         [parent horizontal-2]
         [label "OK"]
         [callback button-callback]))
  
  (preferences:add-on-close-dialog-callback on-save-callback)
  parent)

(define (add-preference-panel)
  (preferences:add-panel
   "Logging tool"
   (lambda (p-frame)
     (make-widgets p-frame #f))))

(define (show-consent-dialog)
  (define frame (new dialog% [label "Research Consent"]))
  (make-widgets frame #t)
  (send frame show #t))
