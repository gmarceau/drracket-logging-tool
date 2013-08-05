#lang scheme

(require lang/run-teaching-program
         lang/htdp-reader
         scheme/sandbox
         scheme/gui/base;
         "loading.rkt"
         gmarceau/hash)

(provide run-item default-teachpacks languages)

(define (make-beginner-reader lang-require)
  (define bodies #f)
  (lambda (name port)
    (match bodies
      [#f
       (let ([stx ((make-read-syntax lang-require) name port)])
         (syntax-case stx ()
           [(module name lang (module-begin))
            (set! bodies empty)
            eof]
           [(module name lang (module-begin b bs ...))
            (set! bodies (syntax->list #'(bs ...)))
            #'b]))]
      [(list) eof]
      [(list f rs ...)
       (set! bodies rs)
       f])))

(define (run-program text lang-require teachpacks)
  (define expandor
    (expand-teaching-program 
     (open-input-value text)
     (make-beginner-reader lang-require)
     lang-require 
     teachpacks #f))
  ;; Once to evaluate the module.
  (define evaluator
    (make-module-evaluator (expandor)))
  ;; Once to get inside the module's namespace.
  (evaluator (expandor))
  ;; Once to trigger any exception that was saved from any of the previous two.
  (expandor)
  evaluator)

(define (wrap-code-with-beginner-metadata text)
  (define lines
    (format "~s\n"
            `((modname '#%htdp)
              (read-case-sensitive #t)
              (teachpacks ())
              (htdp-settings 
               #(#t constructor repeating-decimal #f #t none #f)))))
  (send text insert lines 0))

(define (open-input-value v)
  (let ([p
         (cond [(is-a? v text%)
                (open-input-text-editor v)]
               [(string? v)
                (open-input-file v)]
               [(bytes? v)
                (open-input-bytes v)]
               [else v])])
    (when (port? p) 
      (port-count-lines! p))
    p))

(define (network-guard proc-name hostname port client-or-server)
  (unless (equal? hostname "planet.plt-scheme.org")
    (raise (format "sandbox security: ~a ~a ~a ~a" proc-name hostname port client-or-server))))

(define (with-soft-security thunk)
  (parameterize 
      ([sandbox-path-permissions `((read ,(byte-regexp #"plt-prefs"))
                                   (read ,(byte-regexp #"/logging-tool/analysis/"))
                                   (exists ,(byte-regexp #""))
                                   (write ,(byte-regexp #"^/tmp/")))]
       [sandbox-network-guard network-guard])
    (thunk)))

(define (run-program/capture-all text lang-require teachpacks)
  (define o (open-output-string))
  (define err (open-output-string))
  (parameterize ([sandbox-input        #f]
                 [sandbox-output       o]
                 [sandbox-error-output err])
    (define exn #f)
    (with-handlers
        ([void (lambda (v) (set! exn v))])
      (call-with-trusted-sandbox-configuration
       (lambda () (run-program text lang-require teachpacks))))
    (list (get-output-string o) (get-output-string err) exn)))


(define (run-item item teachpacks)
  (let* ([item (if (hash-has-key? item 'text%)
                   item
                   (load-code item))]
         [text (hash-ref item 'text%)]
         [_ (wrap-code-with-beginner-metadata text)]
         [language-require
          (hash-ref languages (hash-ref item 'lang))]
         [c (make-custodian)]
         [result (parameterize ([current-custodian c])
                   (run-program/capture-all text language-require teachpacks))])
    (custodian-shutdown-all c)
    (match result
      [(list o err exn)
       (hash-set-all item
                     'run-output o
                     'run-error-output err
                     'run-message (and exn (exn-message exn))
                     'run-exn exn)])))

(define default-teachpacks 
  '((lib "world.rkt" "htdp") (lib "dir.rkt" "htdp")))

(define languages
  (make-immutable-hash
   '(("No language chosen" . (lib "htdp-beginner.ss" "lang"))
     ("Beginning Student" . (lib "htdp-beginner.ss" "lang"))
     ("Beginning Student with List Abbreviations" . (lib "htdp-beginner-abbr.ss"))
     ("Intermediate Student" . (lib "htdp-intermediate.ss" "lang"))
     ("Advanced Student" . (lib "htdp-advanced.ss" "lang"))
     ("Intermediate Student with lambda" . (lib "htdp-intermediate-lambda.ss" "lang")))))

(define repl%
  (class* object% ()
    (super-new)
    (define/public (display-results/void v)
      (pretty-print v))
    (define/public (display-untested-summary v)
      (pretty-print v))))

;; two run this, I need to fix
;;    (define (display-untested-summary port)
;; and turn it into
;;    (define/public (display-untested-summary port)
;; in collects/test-engine/test-engine.scm
 
#;
(begin
  (define t (make-object text%))
  (send t insert "(circle 50 'solid 'red) (define (id i) i) (check-expect (id 1) 1)")
  (wrap-code-with-beginner-metadata t)
  (run-program t default-teachpacks)
  
  (define code #"(define (id i) i) (check-expect (id 2) 1)")
  (define item (make-immutable-hash `((text . ,code) (time . #f) (logfile . "logfile"))))
  (define ran (run-item item empty))
  ran
)


