#lang scheme
(require gmarceau/cut
         gmarceau/file
         gmarceau/hash
         gmarceau/list
         mzlib/etc
         srfi/1
         "private/common.rkt"
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
    (define msg (.. item 'exn-message))
    (for/list ([w vocab-words] #:when (regexp-match w msg))
      (list w msg))))

(define matches:h (group-by:h (append* matches) first #:map second))

(define result
  (hash->list
   (hash-map-values:h
    matches:h
    (lambda (lst)
      (sort (remove-duplicates (for/list ([item lst]) (regexp-replace #rx"^[^ ]*: " item "")))
            string<?)))
   #:sort string<?))

(with-saved-output (this-expression-file-name) (lambda () (pretty-print result)))



