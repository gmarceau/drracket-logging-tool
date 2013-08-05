#lang scheme
(require gmarceau/counting
         gmarceau/cut
         gmarceau/debug
         gmarceau/file
         "private/item.rkt")

(define dirname "C:\\Documents\\collects\\analysis\\data")

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

(define matches
  (for*/list ([f (directory-list* dirname)]
              [item (in-log-items f)]
              #:when (?? item 'exn-message))
    (filter
     (// regexp-match <> (.. item 'exn-message))
     vocab-words)))

(define result (count-instances (append* matches)))
result
