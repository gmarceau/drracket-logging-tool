#lang racket
(require gmarceau/test
         gmarceau/cut
         gmarceau/list
         (planet williams/science/statistics)
         (planet williams/science/random-distributions/t-distribution)
         
         (planet williams/science/random-distributions/chi-squared)
         (planet williams/science/random-distributions/gaussian))

(provide (all-defined-out))

(define-struct v/bounds (v lower upper) #:transparent)

(current-test-on? #f)

(define (lift fn)
  (match-lambda*
    [(list (v/bounds av al au) (v/bounds bv bl bu))
     (define lowlow (fn al bl))
     (define lowup (fn al bu))
     (define uplow (fn au bl))
     (define upup (fn au bu))
     (v/bounds (fn av bv) (min lowlow lowup uplow upup) (max lowlow lowup uplow upup))]
    [(list (v/bounds av al au) bv)
     (define on-low (fn al bv))
     (define on-up (fn au bv))
     (v/bounds (fn av bv) (min on-low on-up) (max on-low on-up))]
    [(list av (v/bounds bv bu bl))
     (define on-low (fn av bl))
     (define on-up (fn av bu))
     (v/bounds (fn av bv) (min on-low on-up) (max on-low on-up))]))

(define (in-bounds? bounds v)
  (and (<= (v/bounds-lower bounds) v)
       (<= v (v/bounds-upper bounds))))

(define (plusminus v delta)
  (v/bounds v (- v delta) (+ v delta)))

(test 'lift
      (check-equal? ((lift *) 2 ((lift -) (plusminus 51 1) 1))
                    (v/bounds 100 98 102))
      (check-equal? ((lift *) (plusminus 2 1) ((lift -) (plusminus 51 1) 1))
                    (v/bounds 100 49 153))
      (check-equal? ((lift /) (plusminus 100 40) (plusminus 2 1))
                    (v/bounds 50 20 140)))


(define (stdev-of-ratio r n)
  (/ (* r (- 1 r))
     (sqrt n)))



(define (newton fn y delta #:init [x 0])
  (define fn-x (fn x))
  (if (< (abs (- fn-x y)) delta)
      x
      (let ()
        (define deriv (/ (- (fn (+ x delta)) fn-x)
                         delta))
        (newton fn y delta
                #:init (- x (/ (- fn-x y) deriv))))))

(define newton-accuracy (make-parameter 0.0001))

(define (inv-t-distribution prob degree)
  (newton (lambda (x) (t-distribution-cdf x degree))
          prob newton-accuracy))

(define (inv-gaussian-distribution prob mean stdev)
  (newton (lambda (x) (gaussian-cdf x mean stdev))
          prob newton-accuracy))

(define significance-level (make-parameter 0.05))

(define (bounds-of-average avr stdev n)
  (define (scale x) (- 0 (/ (* x stdev) (sqrt n))))
  (define critical-t (inv-t-distribution (/ (significance-level) 2) (sub1 n)))
  (plusminus avr (scale critical-t)))

(define (bounds-of-normal avr stdev)
  (define cutoff (- 0 (inv-gaussian-distribution (/ (significance-level) 2) 0 1)))
  (plusminus avr (* cutoff stdev)))


(test-data
 (define chi-ex1 '((10 2) (1 11)))
 (define chi-ex2 '((5 12 16) (15 10 7))))

(define (expected-frequencies square)
  (define row-totals (map (// apply + <>) square))
  (define col-totals (map (// apply + <>) (transpose square)))
  (define grand-total (apply + row-totals))
  (for/list ([rt row-totals])
    (for/list ([ct col-totals])
      (/ (* rt ct) grand-total))))
(test 'expected-frequencies
      (check-equal? (mapmap exact->inexact (expected-frequencies chi-ex1))
                    '((5.5 6.5) (5.5 6.5))))

(define (chi-squared-stat square)
  (define expected (expected-frequencies square))
  (apply + (for/list ([obs (flatten square)]
                      [exp (flatten expected)])
             (/ (sqr (- obs exp)) exp))))
(test 'chi-quare-stat
      (check-= (chi-squared-stat chi-ex1) 13.594 0.001)
      (check-= (chi-squared-stat chi-ex2) 8.6902 0.001))


(define (chi-squared-p square)
  (define degrees-of-freedom (* (sub1 (length square))
                                (sub1 (length (first square)))))
  (define stat (chi-squared-stat square))
  (- 1 (chi-squared-cdf stat degrees-of-freedom)))
(test 'chi-square-test
      (check-= (chi-squared-p chi-ex1) 0 0.001)
      (check-= (chi-squared-p chi-ex2) 0.01297 0.001))

