#lang racket

(require gmarceau/all
         ;  (only-in gmarceau/table unique)
         (planet williams/science/statistics)
         "private/common.rkt"
         "private/load-compiles.rkt"
         "private/classifying.rkt"
         "private/lab.rkt"
         "private/stats.rkt"
         mzlib/etc
         racket/set)

(provide (all-defined-out))

(define coding-data-directory "C:\\Documents\\My Dropbox\\error message research - shared\\Experiments\\2010.01 - CS1101 experiment\\Coding")
(define lab1-filename (build-path coding-data-directory "code summary.csv"))
(define other-labs-filename (build-path coding-data-directory "marks - 6.csv"))
(define data-dir "data-processed")

(define input-rubric-fields '(DEL UNR DIFF LIT PART HEST FIX))
(define output-rubric-fields '(DEL UNR DIFF PART FIX))

;;


(define (parse-category str) (read (open-input-string str)))
(define (parse-marks item) (for/first ([f input-rubric-fields] #:when (equal? "x" (.. item f #:default #f))) f))

(define (no-hesitation item)
  (if (eq? (.. item 'code) 'HEST) (!! item 'code 'FIX) item))
(define (no-literally item)
  (if (eq? (.. item 'code) 'LIT) (!! item 'code 'PART) item))

(define (error-name c)
  (format "~a--tab-~a--compile-~a"
          (regexp-replace* #rx"[_ ]+" (.. c 'username) "")
          (pad 2 #\0 (.. c 'tab))
          (pad 3 #\0 (.. c 'compile))))


(define (cleanup-coding c)
  (pipe (!! c 'code (parse-marks c))
        (hash-update <> 'category parse-category)
        (hash-update <> '|error name| (// regexp-replace* #rx"[_ ]+" <> ""))))

(define (update-lab1-coding c)
  (no-literally (no-hesitation c)))

(define (is-a-useful-coding? c)
  (and (?? c 'is-error)
       (?? c 'code)
       (.. c 'code)
       (not (match? c (hash-table ('|code sheet| "1") (k v) ...)))
       (code-was-changed? c)))

(define (non-whitespace-diff? . strings)
  (define collapsed (map (// regexp-replace* #px"[[:space:]]{2,}" <> " ") strings))
  (not (andmap (// equal? (first collapsed) <>) (rest collapsed))))

(define (code-was-changed? c)
  (non-whitespace-diff? (file->bytes (.. c 'filenames 'beginning.rkt))
                        (file->bytes (.. c 'filenames 'end.rkt))))

(define codings-lab1 (csv->table (open-input-file lab1-filename)))
(define codings-other-labs (csv->table (open-input-file other-labs-filename)))

(define cleanup-and-updated
  (remove-duplicates
   (append (map (compose update-lab1-coding cleanup-coding) codings-lab1)
           (map cleanup-coding (table-fill-missing codings-other-labs '|error name| error-name)))
   #:key (// .. <> '|error name|)))

(define (join-compile-with-codings compiles codings)
  (join-on (table-fill-missing compiles '|error name| error-name)
           codings
           '|error name|
           #:missing 'partial
           #:duplicate 'left))

(define compiles-alone (for/list ([c (add-lab (load-compiles data-dir))]
                                  #:when (.. c 'lab))
                         c))
;;;;;;;

;; This is all the compiles, joined with their coding, if any
(define all-compiles
  (pipe (join-compile-with-codings compiles-alone cleanup-and-updated)
        (table-add-column <> 'is-error (// ?? <> 'category))
        (table-add-column <> 'is-coded is-a-useful-coding?)))


(define all-errors (select all-compiles 'is-error #t))

;; This is all the codings that aren't broken in some way (blank, bad rubric, etc)
(define all-codings (select all-errors 'is-coded #t))

(define all-usernames (unique (select all-compiles #:field 'username)))

(define lab-#-of-students (for/list ([lab (get-labs all-compiles)])
                            (length (unique (select lab #:field 'username)))))

;;;;;;;;;


(define (percent-fixed/single codings)
  (define counts (count-instances:h (map (// .. <> 'code) codings)))
  (define denominator (apply + (map (// .. counts <> #:default 0) '(UNR PART FIX))))
  (and (> denominator 0)
       (exact->inexact (/ (.. counts 'FIX #:default 0) denominator))))

(define (percent-fixed codings)
  (define authors (group-by:h codings (// .. <> 'username)))
  (define scores (filter-map percent-fixed/single (hash-values authors)))
  (and (not (empty? scores))
       (apply average scores)))

(define (percent-fixed/err codings)
  (define percentage (percent-fixed codings))
  (and percentage
       (bounds-of-normal percentage
                         (stdev-of-ratio percentage (length codings)))))

(define (-%-of-error-per-student -#-of-students user:-#-of-compiles errors)
  (define s (make-statistics))
  (define users:-#-of-errors (hash-map-values:h (group-by:h errors (// .. <> 'username)) length))
  (if (= -#-of-students 0)
      (plusminus 0 0)
      (begin
        (for ([(u errs) users:-#-of-errors])
          (statistics-tally! s (/ errs (.. user:-#-of-compiles u))))
        (for ([i (in-range 0 (- -#-of-students (hash-size users:-#-of-errors)))])
          (statistics-tally! s 0))
        (bounds-of-average (statistics-mean s)
                           (statistics-standard-deviation s)
                           -#-of-students))))


(define (-%-of-bad-responses-per-sudent -#-of-students user:-#-of-compiles errors codings)
  (define percentage (percent-fixed/err codings))
  (and percentage
       (let ()
         (define count (-%-of-error-per-student -#-of-students user:-#-of-compiles errors))
         ((lift *) count ((lift -) 1 percentage)))))


(define (make-cell lab cat compiles errors)
  (define -#-of-students (length (unique (select compiles #:field 'username))))
  (define codings (select errors 'is-coded #t))
  (define code-counts (for/fold ([result (count-instances:h (select codings #:field 'code))])
                        ([oc output-rubric-fields])
                        (if (?? result oc) result (!! result oc 0))))
  (define user:-#-of-compiles (hash-map-values:h (group-by:h compiles (// .. <> 'username)) length))
  (define -%-of-error (-%-of-error-per-student -#-of-students user:-#-of-compiles errors))
  (define -%-of-bad (-%-of-bad-responses-per-sudent -#-of-students user:-#-of-compiles errors codings))
  
  
  (define result
    (hash 'category cat
          'lab lab
          'compiles (length compiles)
          'errors (length errors)
          'codings (length codings)
          '-#-of-students -#-of-students
          '-%-of-error (v/bounds-v -%-of-error)
          '-%-of-error/lower (v/bounds-lower -%-of-error)
          '-%-of-error/upper (v/bounds-upper -%-of-error)
          '-%-of-bad (and -%-of-bad (v/bounds-v -%-of-bad))
          '-%-of-bad/lower (and -%-of-bad (v/bounds-lower -%-of-bad))
          '-%-of-bad/upper (and -%-of-bad (v/bounds-upper -%-of-bad))
          '-%-fixed (percent-fixed codings)))
  
  (item-join result code-counts))






#|

 (define counts (count-instances:h (map (// .. <> 'code) lst)))
;; Adds zeros for missing categories:
(define filled (for/fold ([result counts]) ([f output-rubric-fields])
                 (hash-update result f id 0))) 



(define number-presented-in-lab1
  (map (match-lambda [(list cat count) (list->hash `((category ,cat) (presented ,count)))])
       (read (open-input-file "out/error-messages-histogram.out"))))

(define summarized (hash-map categories make-summary))
(define joined (sort (table-fill-missing (join-on summarized number-presented-in-lab1 'category #:missing 'partial) 'presented 0) >
                     #:key (// .. <> 'presented)))
(with-saved-output
 (this-expression-file-name) #:extension "csv"
 (lambda () (table->csv joined #:order `(category presented total percent-fixed ,@output-rubric-fields))))
|#


;; for each category, for each lab, what % of student who are taking the lab are hitting that error? :
#;
(define -%-of-student-hitting-that-error
  (for/list ([cat (unique (select errors #:field 'category))])
   (list cat
         (for/list ([(v i) (in-indexed -#-of-student-in-the-lab)])
           (format-percent
            (/ (length (unique (select errors 'category cat 'lab (add1 i) #:field 'username)))
               v))))))



;; For each student, how many distict error message do they read (without the categorization)? :
#;
(define error-message-regexp-per-student
  (sort (for/list ([u (unique (select all-errors #:field 'username))])
          (list u (count-instances (for*/list ([e (select all-errors 'username u)])
                                     (match-define (list (list rx cats ...))
                                                   (classify-file (.. e 'filenames 'rktdata)))
                                     rx))))
        > #:key (lambda (c) (apply + (map counted-c (second c)))) #:cache-keys? #t))

(define all-categories (unique (select all-errors #:field 'category)))
(define cells
  (append
   (for*/list ([cat all-categories]
               [lab (in-labs)])
     (define compiles (select all-compiles 'lab lab))
     (make-cell lab cat compiles (select compiles 'is-error #t 'category cat)))
   (for/list ([lab (in-labs)])
     (make-cell lab '(total) (select all-compiles 'lab lab) (select all-errors 'lab lab)))))

(with-saved-output
 "cells-stats" #:extension "csv"
 (lambda () (table->csv cells #:order '(lab category -#-of-students
                                            compiles errors codings -%-fixed
                                            DEL UNR DIFF PART FIX
                                            -%-of-bad -%-of-bad/lower -%-of-bad/upper
                                            -%-of-error -%-of-error/lower -%-of-error/upper))))








;;----------


(define favorite-categories 
  (set '(arg-count)
       '(runtime cond)
       '(runtime type)
       '(parens matching)
       '(positional&syntax define)
       '(positional&syntax cond)
       '(unbound-id)
       '(positional&syntax struct)
       '(parens meaning-of-open)))


(define short-cells
  (for/list ([c (select cells 'category (// set-member? favorite-categories <>))])
    (define lab-errors (select all-errors 'lab (.. c 'lab)))
    (define lab-#-of-errors (length lab-errors))
    (define h (hash 'category (.. c 'category)
                    'lab (.. c 'lab)
                    'lab-#-of-errors (exact->inexact (/ lab-#-of-errors (.. c '-#-of-students)))
                    '-%-of-error (exact->inexact (/ (length (select lab-errors 'category (.. c 'category)))
                                                    lab-#-of-errors))
                    '-%-bad (- 1 (or (.. c '-%-fixed) 1))))
    (!! h '-#-bad (and (.. h '-%-of-error)
                       (* (.. h 'lab-#-of-errors)
                          (.. h '-%-of-error)
                          (.. h '-%-bad))))))

(with-saved-output
 "short-cells-stats" #:extension "csv"
 (lambda () (table->csv short-cells)))


