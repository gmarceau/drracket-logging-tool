#lang racket
(require gmarceau/all
         (planet williams/science/statistics)
         (planet williams/science/histogram)
         (planet williams/science/histogram-graphics)
         "private/lab.rkt"
         mzlib/etc)

(require "coding_summary_-_how_many_fixes.rkt")

(define wp (table-add-column all-errors 'prob-of-bad (lambda (e) (find short-cells 'lab (.. e 'lab) 'category (.. e 'category) #:field '-%-bad #:default 0))))

(define wp2 (hash-map-values:h (group-by:h wp (lambda (e) (find (list e) #:fields '(username lab))))
                               (lambda (es) (sum (column->list es 'prob-of-bad)))))


(define wp3 (map (lambda (k) (!! k 'prob-of-bad (.. wp2 k))) (hash-keys wp2)))

(for/list ([i (in-labs)])
  (define vs (select wp3 'lab i #:field 'prob-of-bad))
  (list (mean vs) (standard-deviation vs)))


(for/list ([i (in-labs)])
  (define vs (select wp3 'lab i #:field 'prob-of-bad))
  (define h (make-histogram-with-ranges-uniform 10 0 20))
  (for ([v vs]) (histogram-increment! h v))
  (histogram-plot h (format "Histogram: number of bad reponses, Lab ~a" i)))

