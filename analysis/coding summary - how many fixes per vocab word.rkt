#lang scheme
(require gmarceau/counting
         gmarceau/cut
         gmarceau/debug
         gmarceau/file
         gmarceau/hash
         gmarceau/util
         "private/item.rkt"
         (prefix-in codings: "coding_summary_-_how_many_fixes.rkt"))

(define dirname "C:\\Documents\\collects\\analysis\\data--2010-01-20")

(define filenames (map path->string (directory-list dirname)))

(define vocab-words
  (map string-downcase
       '("Function body"
         "Argument"
         "Expression"
         "Type name"
         "Selector"
         "Function header"
         "Identifier"
         "Clause"
         "Primitive operator"
         "Defined name"
         "Field name"
         "Procedure application"
         "Procedure"
         "Predicate"
         "Primitive name")))

(define (message error-name)
  (define rx (format "~a--(.*)" error-name))
  (ormap (match-lambda [(regexp rx (list _ msg)) msg] [_ #f])
         filenames))

(define (vocab-words-of-message msg)
  (filter (// regexp-match <> msg) vocab-words))

(define (add-vocab-words item)
  (!! item 'vocab-words (vocab-words-of-message (message (.. item '|error name|)))))

(define with-vocab (hash-map-values:h codings:categories (// map add-vocab-words <>)))

(define (metric items)
  (define (count-mark mark) (count (match?-lambda "x") (map (// .. <> mark) items)))
  (define denominator (apply + (map count-mark '(UNR PART FIX))))
  (define m (if (= 0 denominator) "n/a" (format-percent (/ (count-mark 'FIX) denominator))))
  (list (length items) m))

(define (has-vocab-word? item)
  (not (empty? (.. item 'vocab-words))))

(define result
  (hash-map-values:h with-vocab
                     (lambda (items) (let-values ([(with without) (partition has-vocab-word? items)])
                                       (map metric (list with without))))))

(define result/percent-fix
  (sort (hash-map result
                  (lambda (cat summary) (list* cat (.. (.. (index-on codings:joined 'category) cat) 'percent-fixed)
                                               summary)))
        string<? #:key second))

result/percent-fix

