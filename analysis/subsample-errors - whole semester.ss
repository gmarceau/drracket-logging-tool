#lang racket

(require (planet bzlib/date/plt)
         "private/load-compiles.rkt"
         "private/item.rkt"
         "private/classifying.rkt"
         "subsample-errors.rkt"
         "private/common.rkt"
         "private/lab.rkt"
         (only-in srfi/1 lset-difference)
         racket/generator
         gmarceau/all)

(define compiles-dir "data-processed")
(define target-dir "shuffled-whole-semester")
(define percentile-desired 85)
(define sample-size 15)

;; ------

(current-test-on? true)


(define (compile-is-a-noop? c)
  (and (?? (.. c 'filenames) 'beginning.rkt)
       (?? (.. c 'filenames) 'end.rkt)
       (= (file-size (.. (.. c 'filenames) 'beginning.rkt))
          (file-size (.. (.. c 'filenames) 'end.rkt)))
       (equal? (file->bytes (.. (.. c 'filenames) 'beginning.rkt))
               (file->bytes (.. (.. c 'filenames) 'end.rkt)))))

(define compiles (filter-not compile-is-a-noop? (load-compiles compiles-dir)))

(define lab-errors (map (// filter (// ?? <> 'category) <>)
                        (get-labs compiles)))

(define category-counts (count-instances:h (column->list (append* lab-errors) 'category)))

(define category-counts-across-labs
  (for/list ([lab lab-errors]) (count-instances:h (column->list lab 'category))))

(define larger-ones
  (map first (take-top-percentile (hash->list category-counts)
                                  (/ percentile-desired 100)
                                  #:key second)))

(define (hash-remove-all-keys-not-in h lst)
  (define to-remove (lset-difference equal? (hash-keys h) lst))
  (for/fold ([result h]) ([k to-remove]) (hash-remove result k)))
(test
 (define h (hash 'a 'a 1 1 'b 'b 'c 'c 2 2))
 (check-match (hash-keys (hash-remove-all-keys-not-in h '(a b c)))
              (list-no-order 'a 'b 'c)))

(define (remove-bad-cats cell) (hash-remove-all-keys-not-in cell larger-ones))

(define cells ;; lab/cat/user/compile
  (map
   (lambda-pipe (group-by:h <> (// .. <> 'category))
                (remove-bad-cats <>)
                (hash-map-values:h <> (// group-by:h <> (// .. <> 'username))))
   lab-errors))

(define (uneven-transpose lstlst)
  (define (cleanup lstlst) (filter-not empty? lstlst))
  (let loop ([lstlst (cleanup lstlst)])
    (cond [(empty? lstlst) empty]
          [else
           (cons (map first lstlst)
                 (loop (cleanup (map rest lstlst))))])))
(test (check-equal? (uneven-transpose '((0 1 2 3 4)
                                        (10 11 12 13 14 15)
                                        (20 21 22)
                                        ()
                                        (30 31 32 33 34 35)))
                    '((0 10 20 30)
                      (1 11 21 31)
                      (2 12 22 32)
                      (3 13 33)
                      (4 14 34)
                      (15 35))))


(define (subsample-cell cell n)
  (define lst (append* (uneven-transpose (shuffle (map shuffle (hash-values cell))))))
  (take lst (min n (length lst))))

(define sampled (map (// hash-map-values:h <> (// subsample-cell <> sample-size)) cells))



;; --------


(ensure-empty-dir target-dir)

(define (in-sampled)
  (apply in-parallel
         (transpose (for*/list ([(lab i) (in-indexed sampled)]
                                [(cat compiles) lab]
                                [c compiles])
                      (list i cat c)))))

(define (ensure-dir dir)
  (or (directory-exists? dir) (make-directory dir)))

(for ([(lab-i cat compile) (in-sampled)])
  (define lab-dir (build-path target-dir (format "lab-~a" (add1 lab-i))))
  (define cat-dir (build-path lab-dir (format "~a" cat)))
  (ensure-dir lab-dir)
  (ensure-dir cat-dir)
  (for ([filename (hash-values (.. compile 'filenames))])
    (copy-file filename (build-path cat-dir (path-filename filename)))))

#;
(for ([(lab i) (in-indexed sampled)])
  (define lab-dir (build-path target-dir (format "lab-~a" (add1 i))))
  (make-directory lab-dir)
  (for ([(cat compiles) lab])
    (define cat-dir (build-path lab-dir (format "~a" cat)))
    (make-directory cat-dir)
    (for* ([c compiles] [filename (hash-values (.. c 'filenames))])
      (copy-file filename (build-path cat-dir (path-filename filename))))))


(let ()
  (with-output-to-file
      (build-path target-dir "marks.csv") #:exists 'replace
    (lambda ()
      (printf "lab, category, username, tab, compile~n")
      (for ([(lab-i cat compile) (in-sampled)])
        (printf "~a, ~a, ~a, ~a, ~a~n"
                lab-i cat
                (.. compile 'username)
                (.. compile 'tab)
                (.. compile 'compile))))))





