#lang scheme

(require gmarceau/table
         gmarceau/cut
         gmarceau/list
         gmarceau/hash
         gmarceau/util)



(define filename "C:\\Documents\\My Dropbox\\error message research - shared\\CS1101 experiment - 2010.01\\Coding\\code summary - poster.csv")

(define data (csv->table (open-input-file filename)))

(define categories (group-by:h data (// .. <> 'category)))
(define fixed-field-name '|Fixed (w or w/ hesitation)|)

(define (was-fixed? item)
  (equal? (.. item fixed-field-name) "TRUE"))

(define (parse str)
  (read (open-input-string str)))

(define result
  (hash->list
   (hash-map-keys:h
    (hash-map-values:h
     categories
     (lambda (lst)
       (define a (length (filter was-fixed? lst)))
       (define b (length lst))
       (list a b (format-percent (/ a b)))))
    parse)))

(define sorted
  (sort result > #:key
        (match-lambda
          [(list _ (list _ num _)) num])))
sorted
