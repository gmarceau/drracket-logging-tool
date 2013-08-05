#lang racket
(require "private/load-compiles.rkt"
         "private/lab.rkt"
         "private/common.rkt"
         mzlib/etc
         gmarceau/all)

(define dir "data-processed")

;; ---

(define compiles (table-fill-missing (load-compiles dir) 'category #f))

(define categories (remove-duplicates (column->list compiles 'category)))

(define (count-categories cs)
  (for/list ([cat categories]) (length (select cs 'category cat))))

(define result (transpose (map count-categories (get-labs compiles))))

(with-saved-output
 (this-expression-file-name)
 #:extension "csv"
 (lambda ()
   (for ([cat categories]
         [r result])
     (printf "~a~n" (string-join (map (// format "~a" <>) (cons cat r)) ",")))))

