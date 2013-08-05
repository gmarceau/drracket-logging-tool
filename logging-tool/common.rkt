#lang scheme

(provide (all-defined-out))

(define log-filename
  (make-parameter (build-path (find-system-path 'pref-dir) (format "drscheme-run.log"))))
(define file-semaphore (make-semaphore 1))

(define server-host (make-parameter "kfisler-ra2.cs.wpi.edu"))
(define server-port-number 8088)

(define mute-client-failures? #t)

(define mute-all-exceptions #t)

(define (with-maybe-silent-failure mute? thunk)
  (if mute?
      (with-handlers ([void void])
        (thunk))
      (thunk)))

(define (with-maybe-printed-failures mute? name thunk)
  (with-handlers
      ([void
        (lambda (exn)
          (unless mute?
            (pretty-print `(,name ,(exn-message exn)))))])
    (thunk)))

(define (ignore v) (void))

#;
(current-debug-printer
 (let ([old-printer (current-debug-printer)])
   (lambda (msg v)
     (old-printer msg v)
     (flush-output))))