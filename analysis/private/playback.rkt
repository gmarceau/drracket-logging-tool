#lang scheme/gui

(require gmarceau/cut)

(define-struct record (timestamp operation start len content) #:prefab)
(provide (struct-out record))

(provide/contract [apply-record/txt ((is-a?/c text%) record? . -> . void?)]
                  [apply-records/txt ((is-a?/c text%) (listof record?) . -> . void?)]
                  [apply-records ((listof record?) . -> . (is-a?/c text%))])

(define (apply-record/txt txt r)
  (match (record-operation r)
    ['insert (send txt insert (record-content r) (record-start r))]
    ['on-delete (send txt delete (record-start r) (+ (record-start r) (record-len r)))]))

(define (apply-records/txt txt rs)
  (for ([r rs]) (apply-record/txt txt r)))

(define (apply-records rs)
  (define txt (new text%))
  (apply-records/txt txt rs)
  txt)
