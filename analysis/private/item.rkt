#lang scheme

(require gmarceau/cut
         gmarceau/hash
         gmarceau/table
         )

(provide item-of-type?
         filter-by-type
         item->list
         ..
         ??
         !!
         --
         item?
         item/c
         item-kv/c
         )

(define (item-type item) (hash-ref item 'event))

(define (item-of-type? evt type)
  (eq? (hash-ref evt 'event) type))

(define (filter-by-type type items)
  (filter (// item-of-type? <> type) items))

(define (item->list item)
  (sort (hash->list item) string<=? #:key (compose symbol->string first)))





