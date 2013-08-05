#lang scheme
(require gmarceau/cut
         gmarceau/file
         gmarceau/hash
         gmarceau/list
         gmarceau/util
         mzlib/etc
         srfi/1
         "private/loading.rkt"
         "private/common.rkt"
         "private/classifying.rkt"
         "private/item.rkt")

(define dirname "C:\\Documents\\collects\\analysis\\data")

(define msgs
  (for*/list ([f (take (directory-list* dirname) 2)]
              [item (in-log-items/no-repl f)]
              #:when (?? item 'exn-message))
    (.. item 'exn-message)))

(define result
  (hash-map-values (group-by:h msgs error-message-category)
                   (compose remove-duplicates
                            (// sort <> string<=?))))

(with-saved-output (this-expression-file-name) (lambda () (pretty-print result)))

result

