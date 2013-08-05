#lang scheme

(require gmarceau/hash
         gmarceau/counting
         gmarceau/file
         gmarceau/list
         gmarceau/debug
         (planet untyped/unlib/for)
         mzlib/etc
         "private/loading.rkt"
         "private/common.rkt"
         "private/classifying.rkt"
         "private/item.rkt")

#;
(define src-dirname "data")

(define filenames '("data\\drscheme-tool-log-entries-received-2010-01-20"))

;; ---------
#;
(define filenames
  (for/list ([f (directory-list* src-dirname)]
             #:when (not (equal? (string->path "gz") (path-extension f))))
    f))

(define skip-next #f)

(define error-messages
  (for/fold/reverse
   ([result empty])
   ([f filenames])
   
   (for/fold1 ([result result] [previous-was-repl #f])
              ([item (in-log-items (%%v f))])
     (cond
       [(eq? 'repl (hash-ref item 'event))
        (values result #t)]
       [previous-was-repl
        (values result #f)]
       [(eq? (hash-ref item 'event) 'compile-error)
        (values result #f)]
       [(hash-has-key? item 'exn-message)
        (let ()
          (define msg (hash-ref item 'exn-message))
          (define c (classify-error-message msg))
          (match c
            [(list) (pretty-print (list 'unclassified msg))]
            [(list fst snd rst ...) (pretty-print (list 'many-classes c msg))]
            [_ (void)])
          (values (cons msg result) #f))]
       [else (values result #f)]
       ))))

(define classified (map classify-error-message error-messages))

(define counted-regexp (count-instances classified))

(define rearranged (map
                    (match-lambda [(struct counted ((list (list rx cats ...)) c)) `(,cats (,c ,rx))]
                                  [(struct counted ((list) c)) `('no-cat ,c)])
                    counted-regexp))

(define histo (error-message-histogram error-messages))

(require (planet untyped/unlib/match))

(define table
  (for/list ([c histo])
    `(,(counted-c c)
      ,@(counted-cats c)
      ,@(filter-map
         (match-lambda [(list (equal? (counted-cats c)) (list c rx))
                        (list c rx)]
                       [_ #f])
         rearranged))))

(define grouped (group-by:h error-messages
                           (lambda (msg)
                             (match (classify-error-message msg)
                               [(list) empty]
                               [(list (list rx cats ...)) cats]))))

(define detailed-result (sort (hash->list (hash-map-values:h grouped (lambda (es) (sort es string<?))))
                              > #:key (compose length second) #:cache-keys? #t))

(define summary-result (sort (hash->list (hash-map-values:h grouped length)) < #:key second))

(with-saved-output
 (this-expression-file-name)
 (lambda () (pretty-print summary-result)))
summary-result