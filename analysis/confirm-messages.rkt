#lang scheme
(require gmarceau/cut
         "private/common.rkt"
         "private/running.rkt"
         "private/item.rkt")

;; This program run through all the log files in ./data-processed-02,
;; find all the items with code attached, executes the code, then
;; confirms that the same error message is generated as the one which
;; was originally recorded.


(define target-dirname (make-target-dirname 2))

(define (convert-snip s)
  (cond [(bytes? s) s]
        [(string? s) (string->bytes/utf-8 s)]
        [else #"'x"]))

(define (convert-text text)
  (apply bytes-append (map convert-snip text)))

(define (process filename)
  (define items (load-log filename))
  (define added (map
                 (compose
                  (// hash-set <> 'lang "Beginning Student")
                  (// hash-set <> 'logfile filename)
                  )
                 items))
  (define compiles (filter-by-type 'compile added))
  (define converted (map (// hash-update <> 'text convert-text) compiles))
  (define msgs
    (for/list ([i items] #:when (hash-has-key? i 'exn-message))
      (.. i 'exn-message)))
  (define ran (map (// run-item <> empty) converted))
  (define ran-message (filter (lambda (i) i) (map (// .. <> 'run-message) ran)))
  
  (when (not (equal? ran-message msgs))
    (pretty-print filename)
    (pretty-print ran-message)
    (pretty-print msgs)
    (newline)))

(define filenames (directory-list target-dirname))
(current-directory target-dirname)
(for-each process filenames)