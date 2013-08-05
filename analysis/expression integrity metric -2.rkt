#lang racket
(require gmarceau/all
         gmarceau/date-time
         "private/common.rkt"
         "private/lab.rkt"
         "private/load-compiles.rkt"
         "private/classifying.rkt"
         "private/pic.rkt"
         (planet untyped/unlib/match)
         mrmathematica/main
         (only-in srfi/1 take-while)
         unstable/function
         plot
         (planet williams/science/histogram)
         (planet williams/science/histogram-graphics)
         (planet williams/science/statistics))

(define Math MathEval)
(define Math/i Mexp->image)

(define data (eval (with-input-from-file "out/expression integrity metric.out.rktdata" read)
                   (make-base-namespace)))

(define pros (.. data 'pros))
(define students (.. data 'students))

(define (split-at-zeros lst)
  (cond [(empty? lst) empty]
        [(= 0 (first lst))
         (cons empty (split-at-zeros (rest lst)))]
        [else
         (match (split-at-zeros (rest lst))
           [(list) (list (list (first lst)))]
           [(list fst rst ...) (cons (cons (first lst) fst) rst)])]))

(current-test-on? #f)
(test
 (check-equal? (split-at-zeros '(0)) '(()))
 (check-equal? (split-at-zeros '(1 1 1 0 2 2 2 0 3 3 3 0 0))
               '((1 1 1) (2 2 2) (3 3 3) ())))

(define (walk-length-metric unbalancings)
  (mean (map length (filter-not empty? (split-at-zeros unbalancings)))))

(define (sum lst) (apply + lst))

(define (walk-weight-metric unbalancings)
  (mean (map sum (split-at-zeros unbalancings)))
  #;
  (/ (mean (map sum (split-at-zeros unbalancings)))
     (length unbalancings)))

(define (compiles:walk-length-metric cs)
  (pipe (column cs 'unbalancings)
        (append* <>)
        (walk-length-metric <>)))

(define (compiles:walk-weight-metric cs)
  (pipe (column cs 'unbalancings)
        (append* <>)
        (walk-weight-metric <>)))

(define (mean-and-std lst)
  (list (histogram #:n 30 lst #:max 30)
        (mean lst)
        (standard-deviation lst)))

(define (mean-of-walk-weights users)
  (mean-and-std (map
                 (lambda-pipe (column <> 'unbalancings)
                              (append* <>)
                              (walk-weight-metric <>))
                 (hash-values users))))

(define (mean-of-walk-lengths users)
  (mean-and-std
   (map (lambda-pipe (column <> 'unbalancings)
                     (append* <>)
                     (walk-length-metric <>))
        (hash-values users))))

(define students-successful
  (hash-map-values:h students (// filter-not (// ?? <> 'category) <>)))

(define (summary name users)
  (hash 'name name
        'walk-length (mean-of-walk-lengths users)
        'walk-weight (mean-of-walk-weights users)))

(define (histogram lst #:label [title ""] #:n [n 10] #:min [min (minimum lst)] #:max [max (add1 (maximum lst))] #:with-mean [with-mean #f])
  (define h (make-histogram-with-ranges-uniform n min max))
  (for ([v lst]) (histogram-increment! h v))
  (histogram-plot h (if with-mean (format "~a (mean: ~a)" title (mean lst)) title)))
#;
(begin
  (summary 'pros pros)
  (summary 'students students-successful))

(define (plot-points2 xs ys #:x-min [x-min (minimum xs)]
                      #:x-max [x-max (maximum xs)]
                      #:y-min [y-min (minimum ys)]
                      #:y-max [y-max (maximum ys)])
  (plot (points (map vector xs ys))
        #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max))

(define (plot-points ps #:x-min [x-min (minimum (map first ps))]
                     #:x-max [x-max (maximum (map first ps))]
                     #:y-min [y-min (minimum (map second ps))]
                     #:y-max [y-max (maximum (map second ps))])
  (plot (points (map list->vector ps))
        #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max))

;-------------


(define (texts-unbs c)
  (for/list ([text (.. c 'texts)]
             [unb (.. c 'unbalancings)])
    (item-join
     (hash 'text text 'unbalancing unb)
     (item-select c 'username 'tab 'compile))))

(define (worse-n n items)
  (take (sort (unique items #:key (// .. <> 'text))
              > #:key (// .. <> 'unbalancing))
        n))

(define (all-pairwise-different? lst)
  (for/and ([i lst]
            [prev (cons #f lst)])
    (not (equal? i prev))))

(define (save dirname items)
  (ensure-empty-dir dirname)
  (for ([(item i) (in-indexed items)])
    (with-output-to-file
        (build-path dirname
                    (regexp-replace* "#hash" (format "~a -- ~a.rkt" (pad 2 #\0 i) (-- item 'text 'unbalancing)) ""))
      (lambda () (display (.. item 'text))))))
#;
(begin
  (define worse-intermediate (worse-n 50 (append* (map texts-unbs (append* (hash-values students))))))
  (define worse-compiles (worse-n 10 (map first (filter-not empty? (map texts-unbs (append* (hash-values students)))))))
  (save "out/worse-intermediate" worse-intermediate)
  (save "out/worse-compile" worse-compiles)
  )
(define (no-uscore u) (regexp-replace* "_" u ""))
(define grades (call-with-input-file "data\\CS1101 - C10 - Grades\\1101-C10-study-grades.csv" csv->table))

(define student-data
  (for/list ([(u cs) students])
    (hash 'username u
                 'grades
                 (match (select grades 'Username (lambda (uu) (equal? (no-uscore u) (no-uscore uu))))
                   [(list x) x]
                   [(list) #f])
                 'n-presences
                 (count (lambda (lab) (not (empty? lab))) (get-labs cs))
                 'compiles cs)))


(define (mrmath-plot/fit data:lst)
  (define data (list->vector (map list->vector data:lst)))
  (Math `(Set lm (LinearModelFit ,data x x)))
  (Math/i `(Show (ListPlot ,data (Rule PlotMarkers #(Automatic 16))) (Plot (lm x) #(x 1 6)))))
;; each student over time

(define (mrmath-fit-slope data:lst)
  (define data (list->vector (map list->vector data:lst)))
  (Math `(Part ((LinearModelFit ,data x x) "BestFitParameters") 2)))

(define (unbalancing-over-time compiles)
  (for/list ([(lab i) (in-indexed (get-labs compiles))]
             #:when (> (length lab) 5))
    (list i (compiles:walk-weight-metric lab))))

(define (plot-of-unbalancings data)
  (if (> (length data) 4)
      (mrmath-fit-slope data)
      #f))


;; all the last edits of students for more than 3 presences
#;
(let ()
  (define out-dir "out/last-edit")
  (ensure-empty-dir out-dir)
  (for* ([u (select student-data 'n-presences (// > <> 3))]
         [(lab i) (in-indexed (get-labs (.. u 'compiles)))])
    (and (not (empty? lab))
         (let ()
           
           (for ([(_ c) (hash-map-values:h (table-group-by lab 'tab)
                                           (compose last (// sort <> date<? #:key (// .. <> 'time))))])
             (define filename
               (.. (find compiles 'username (.. c 'username) 'compile (.. c 'compile) 'tab (.. c 'tab))
                   'filenames 'end.rkt))
             (define dest (format "out/last-edit/lab-~a-~a" (add1 i) (path-filename filename)))
             (copy-file filename dest)
             (file-or-directory-modify-seconds dest (file-or-directory-modify-seconds filename)))))))

(define present-students
  (list->hash (for/list ([u (select student-data 'n-presences (// > <> 3) #:field 'username)])
                (list u (.. students u)))))

(require slideshow/pict racket/draw)

;; sparklines for pros
#;
(save-to-png (apply vl-append 40 
                    (for/list ([(u cs) pros])
                      (bitmap (send (Math/i `(ListLinePlot #(,@(append* (column cs 'unbalancings)))
                                                           (Rule ImageSize #(400 100))
                                                           (Rule PlotRange #(0 5))
                                                           (Rule AspectRatio Full)
                                                           (Rule Axes False)))
                                    get-bitmap))))
             "out/sparklinespros.png")

(define (make-sparkline cs)
  (define rectangle-for-success (filled-rectangle 10 36))
  (define rectangle-for-error (pin-over (pin-over (rectangle 10 36) 0 0 (pip-line 9 35 1))
                                        0 35 (pip-line 9 -35 1)))
  (if (or (empty? cs) (empty? (append* (column cs 'unbalancings))))
      (blank 450 300)
      (apply hb-append
             (for/list ([resp cs])
               (hb-append
                (if (?? resp 'category) rectangle-for-error rectangle-for-success)
                (if (empty? (.. resp 'unbalancings))
                    (blank)
                    (bitmap (send (Math/i `(ListLinePlot #(,@(.. resp 'unbalancings))
                                                         (Rule ImageSize #(,(* 3 (length (.. resp 'unbalancings))) 300))
                                                         (Rule PlotRange #(0 17))
                                                         (Rule AspectRatio Full)
                                                         (Rule Axes False)))
                                  get-bitmap))))))))

(define (save-sparkline filename-label pict)
  (save-to-png
   (lt-superimpose (colorize (filled-rectangle (pict-width pict) (pict-height pict)) "white") pict)
   (format "out/sparkline-~a.png" filename-label)))

(define (make-sparkline/lab cs)
  (define charts (map make-sparkline (get-labs cs)))
  (apply hb-append (add-between charts (rectangle 50 300))))

;; spark lines for students
#;
(for ([(u cs) present-students])
  (pretty-print u)
  (save-sparkline (format "student-~a-~a" (round (string->number (.. (find student-data 'username u) 'grades '|Final Avg|))) u)
                  (make-sparkline/lab cs)))
#;
(save-sparkline "pros" (table 2
                              (for/list ([cs (hash-values pros)])
                                (define chart (inset/clip (make-sparkline cs) 0 -200 0 0))
                                (vl-append chart (colorize (hline (pict-width chart) 1) "gray")))
                              lb-superimpose lb-superimpose 100 5))


(define (student-final-grade username)
  (define g (.. (find student-data 'username username) 'grades))
  (and g (string->number (.. g '|Final Avg|))))

;; histograms across time. 
#;
(map (// histogram <> #:max 80) (transpose (hash-values (hash-map-values:h present-students
                                                                           (lambda (cs) (map compiles:walk-weight-metric
                                                                                             (get-labs (filter (// ?? <> 'category) cs))))))))

(define (compile-name item) (list (.. item 'username) (.. item 'tab) (.. item 'compile)))
(define compiles (table-add-column (load-compiles "data-processed\\CS1101 - C10") 'name compile-name))

(define (is-balanced? c)
  (not (equal? (.. c 'category #:default #f) '(parens matching))))

(define (is-error? c) (and (?? c 'category) #t))

(define (head-to-first-wb cs)
  (take-while (negate is-balanced?) cs))

(define (group-from-wb-to-wb cs)
  (match cs
    [(list) empty]
    [(list fst rst ...)
     (when (not (is-balanced? (first cs))) (error "starts with unbalanced"))
     (define hd (head-to-first-wb rst))
     (cons (cons fst hd) (group-from-wb-to-wb (drop rst (length hd))))]))
(test
 (define d (for/list ([i '(ok x x x ok x x ok ok ok x x x ok)])
             (if (eq? 'ok i) (hash) (hash 'category '(parens matching)))))
 (check-equal? (mapmap is-balanced? (group-from-wb-to-wb d))
               '((#t #f #f #f) (#t #f #f) (#t) (#t) (#t #f #f #f) (#t))))

(define (is-trivial-run? run)
  (match? (unique (append* (column run 'unbalancings)))
          (or (list) (list _))))

(define class-average (mean (map string->number (column (filter (lambda (i) i) (column student-data 'grades)) '|Final Avg|))))

(define (student-is-top-half? username)
  (and (student-final-grade username)
       (> (student-final-grade username) class-average)))

;; a response run starts from a successful F5 or a non-paren error,
;; and run until a successful F5 or non-paren error (it includes all the parens error
;; in between.
(define (response-runs users)
  (hash-map-values:h
   users
   (lambda-pipe (sort <> date<? #:key (// .. <> 'time))
                (group-from-wb-to-wb <>)
                (filter (negate is-trivial-run?) <>))))

(define (gather-response-runs users pred)
  (filter (compose pred first) (append* (hash-values (response-runs users)))))

(define (filter-users users pred)
  (for/fold ([result empty-hash]) ([(u cs) users] #:when (pred u))
    (hash-set result u cs)))

(define response-runs/from-succesful (gather-response-runs present-students (negate is-error?)))
(define response-runs/from-error (gather-response-runs present-students (conjoin is-error? is-balanced?)))
(define response-runs/from-pros (gather-response-runs pros (lambda (i) #t)))

(define (run-which-lab run) (which-lab? (first run)))

(define (histogram-of-walk-weight-of-runs runs)
  (define ms (map compiles:walk-weight-metric runs))
  (histogram #:n 50 ms #:max 50 #:with-mean #t))

(define (get-labs-of-runs runs)
  (map second (sort (filter first (group-by runs run-which-lab)) < #:key first)))

(define (histogram-of-walk-weight-of-runs/across-labs runs)
  (apply hb-append 20
         (for/list ([runs (get-labs-of-runs runs)])
           (bitmap (send (histogram-of-walk-weight-of-runs runs) get-bitmap)))))

; ---
;; histogram of the distribution of the metric
#;
(list
 (histogram-of-walk-weight-of-runs response-runs/from-succesful)
 (histogram-of-walk-weight-of-runs response-runs/from-error)
 (histogram-of-walk-weight-of-runs response-runs/from-pros)
 )

;---
; correlation with final grades

(define (student-number-of-error-of-kind username category)
  (number-of-error-of-kind (.. students username) category))

(define (number-of-error-of-kind cs category)
  (length (filter (match?-lambda (hash-table ['category (list (equal? category) rst ...)] [_ _] ...)) cs)))

(define (correlate ps)
  (define ps:lst (map list->vector ps))
  (Math `(Set lm (LinearModelFit #(,@ps:lst) x x)))
  (list (Math/i '(lm "ANOVATable"))
        (Math '(lm "RSquared"))
        (Math/i `(Show (ListPlot #(,@ps:lst)) (Plot (lm x) #(x 0 50))))))

(define (correlate-runs runs dependant)
  (define per-user (group-by:h runs (lambda (r) (.. (first r) 'username))))
  
  (correlate
   (for/list ([(u u-runs) per-user]
              #:when (dependant u))
     (list (mean (map compiles:walk-weight-metric u-runs)) (dependant u)))))

#;
(begin
  (correlate-runs response-runs/from-succesful student-final-grade)
  (correlate-runs response-runs/from-error student-final-grade)
  (correlate (for/list ([u (hash-keys present-students)] #:when (student-final-grade u))
               (list (student-number-of-error-of-kind u 'parens) (student-final-grade u)))))

(define n-present-students (length (hash-keys present-students)))


; - correlation across kinds of error messages
#;
(for/list ([cat (unique (map first all-error-categories))]
           #:when (> (mean (map (// number-of-error-of-kind <> cat ) (hash-values present-students)))
                     5))
  (list cat (correlate-runs response-runs/from-succesful (// student-number-of-error-of-kind <> cat))))

; ----


(define fit-data2
  (for/list ([(u u-runs) (group-by:h response-runs/from-succesful (lambda (r) (.. (first r) 'username)))])
    (vector (student-number-of-error-of-kind u 'parens)
            (mean (map compiles:walk-weight-metric u-runs))
            (student-final-grade u))))
(Math/i `((LinearModelFit #(,@fit-data2) #(parenserrs metric) #(parenserrs metric)) "ANOVATable"))








