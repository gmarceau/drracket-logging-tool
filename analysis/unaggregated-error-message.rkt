#lang racket
(require gmarceau/all
         "private/load-compiles.rkt"
         "private/lab.rkt"
         )


(define targets
  (map (// build-path "data-processed" <>)
       '("data--2010-01-20"
         "data--2010-01-27"
         "data--2010-02-04"
         "data--2010-02-10"
         "data--2010-02-16"
         "data--2010-02-24")))


(define compiles-with-message
  (for/list ([c (map load-compiles targets)]
             #:when (?? c 'messages))
    c))

(for/list ([u (remove-duplicates (column->list compiles-with-message 'username))])
  (list u
        (for/list ([i (in-range 1 7)])
          (length (remove-duplicates (column->list (select compiles-with-message 'username u 'lab i) 'message))))))

