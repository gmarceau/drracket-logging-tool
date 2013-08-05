#lang racket
(require (planet bzlib/date/plt)
         (only-in srfi/1 lset-difference)
         gmarceau/all)

(provide (struct-out interval)
         get-lab
         get-labs
         add-lab
         in-labs
         which-lab?)

(current-test-on? false)
(struct interval (start end))

(provide/contract [is-in-interval? (interval? date? . -> . boolean?)])
(define (is-in-interval? int date)
  (and (date<=? (interval-start int) date)
       (date<=? date (interval-end int))))

(define timezone -18000)

(provide/contract [lab-intervals (parameter/c (listof interval?))])
(define lab-intervals
  (make-parameter
   (let ()
     (define (d month day)
       (interval
        (build-date 2010 month day 9 #:tz timezone)
        (build-date 2010 month day 15 #:tz timezone)))
     (list (d 1 20)
           (d 1 27)
           (d 2 3)
           (d 2 10)
           (d 2 17)
           (d 2 24)))))

(provide -#-of-labs in-labs)
(define (-#-of-labs) (length (lab-intervals)))
(define (in-labs) (in-range 1 (add1 (-#-of-labs))))


(test "is-in-interval?"
      (check-true (is-in-interval? (first (lab-intervals)) (build-date 2010 1 20 11 #:tz timezone)))
      (check-false (is-in-interval? (first (lab-intervals)) (build-date 2010 1 20 7 #:tz timezone)))
      (check-false (is-in-interval? (first (lab-intervals)) (build-date 2010 1 20 20 #:tz timezone))))

(provide/contract [filter-by-date ((table/c 'time) interval? . -> . (table/c 'time))])
(define (filter-by-date compiles int)
  (select compiles 'time (// is-in-interval? int <>)))

(define (has-lab? compiles)
  (or (empty? compiles) (?? (first compiles) 'lab)))

(define (ensure-has-lab compiles)
  (if (has-lab? compiles) compiles (add-lab compiles)))

(define (get-lab compiles lab-number)
  (select (ensure-has-lab compiles) 'lab lab-number))

(define (get-labs compiles)
  (define ec (ensure-has-lab compiles))
  (build-list (length (lab-intervals))
              (lambda (i) (select ec 'lab (add1 i)))))

(define (which-lab? compile)
  (.. compile 'lab
      #:default (lambda () (for/first ([(interval i) (in-indexed (lab-intervals))]
                                       #:when (is-in-interval? interval (.. compile 'time)))
                             (add1 i)))))

(define (add-lab compiles)
  (for/list ([c compiles]) (!! c 'lab (which-lab? c))))
