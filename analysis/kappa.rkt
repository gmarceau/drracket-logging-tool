#lang scheme


(require srfi/1
         gmarceau/cut
         gmarceau/hash
         gmarceau/util
         gmarceau/list
         gmarceau/counting
         (planet neil/csv:1:5/csv))

(define base "C:\\Documents\\My Dropbox\\error message research - shared\\CS1101 experiment - 2010.01\\coding")
(define first-filename (build-path base "marks - 4 - by-gmarceau.csv"))
(define second-filename (build-path base "marks - 4 - by-kathi.csv"))

(define simplifications (list->hash '((2 1) (3 1) (4 1) (5 6))))

;; ----------


(define-struct coding (error-type name notes error-code response-code) #:transparent)

(define coding-data/c (listof (listof coding?)))
(define coding-numbers/c (listof (listof number?)))
(define simplification/c (hash/c number? number?))

;; Takes a list of blank or marks and return the position of the first "x"
(define/contract (marks->number lst)
  ((listof string?) . -> . (or/c number? false?))
  
  (for/first ([item lst] [i (in-naturals)] #:when (equal? item "x"))
    i))

;; Open a .csv file of codings that has the layout of the xls file
(define/contract (open-code-csv filename)
  (path? . -> . (listof coding?))
  
  (define data (csv->list (open-input-file filename)))
  (define data2 (drop data 2))
  (map (match-lambda
         [(list error-type name notes tail ...)
          (make-coding (read (open-input-string error-type))
                       name notes
                       (marks->number (take tail 4))
                       (marks->number (drop tail 4)))])
       data2))


(define/contract (complete? c)
  (coding? . -> . boolean?)
  
  (and (coding-error-code c) (coding-response-code c) #t))

(define/contract (complete-pair? c1 c2)
  (coding? coding? . -> . boolean?)
  
  (and (complete? c1) (complete? c2)))

(define/contract (open-coding-pair first-filename second-filename)
  (path? path? . -> . (listof (list/c coding? coding?)))
  
  (define fst (open-code-csv first-filename))
  (define snd (open-code-csv second-filename))
  (define fst:h (list->hash (zip (map coding-name fst) fst)))
  (for/list ([item snd])
    (list (hash-ref fst:h (coding-name item)) item)))


(define (delta a b #:key [key-fn id])
  (abs (- (key-fn a) (key-fn b))))

(define/contract (delta-error-code c1 c2)
  (coding? coding? . -> . number?)
  (delta #:key coding-error-code c1 c2))

(define/contract (delta-response-code c1 c2)
  (coding? coding? . -> . number?)
  (delta #:key coding-response-code c1 c2))

(define/contract (all-the-same? lst)
  (list? . -> . boolean?)
  (= 1 (length (remove-duplicates lst))))

(define/contract (summarize data)
  (coding-data/c . -> . any/c)
  (map
   (match-lambda
     [(list c1 c2) (list (coding-response-code c1)
                         (coding-response-code c2)
                         'delta
                         (delta-response-code c1 c2)
                         (coding-name c1))])
   
   (sort-by-delta data)))

(define (sort-by-delta data)
  (coding-data/c . -> . coding-data/c)
  (let ()
    (define s1
      (for/fold ([result data]) ([op (list second first)])
        (sort result < #:key (compose coding-response-code op))))
    
    (sort s1 <
          #:key (match-lambda [(list c1 c2) (delta-response-code c1 c2)]))))

;; -----

(define data (open-coding-pair first-filename second-filename))

(define complete-data (filter (match-lambda [(list c1 c2) (complete-pair? c1 c2)]) data))

(define sorted (sort-by-delta complete-data))

(define summary (summarize sorted))
'summary
summary

(define common-confusions
  (count-instances
   (filter-not all-the-same?
               (map (// sort <> <)
                    (map (// take <> 2) summary)))))
'common-confusions
common-confusions

(define (simplify-v v [simplifications simplifications])
  (hash-ref simplifications v (lambda () v)))

(define (simplify-coding c [simplifications simplifications])
  (struct-copy coding c
               [response-code (simplify-v (coding-response-code c) simplifications)]))

(define/contract (simplify-data data [simplifications simplifications])
  ([coding-data/c] [simplification/c] . ->* . coding-data/c)
  (mapmap (// simplify-coding <> simplifications) data))

(define/contract (prob-of-v v vs)
  (any/c list? . -> . number?)
  (/ (count (lambda (i) (equal? i v)) vs)
     (length vs)))

(define/contract (prob-of-agreement data)
  (coding-numbers/c . -> . number?)
  (/ (length (filter all-the-same? data))
     (length data)))

(define/contract (codings->numbers cs field-fn)
  (coding-data/c (coding? . -> . number?) . -> . coding-numbers/c)
  (for/list ([c cs])
    (list (field-fn (first c)) (field-fn (second c)))))

(define/contract (kappa data)
  (coding-numbers/c . -> . number?)
  (define choices (remove-duplicates (flatten data)))
  (define expected-v
    (for/list ([c choices])
      (for/list ([person (transpose data)])
        (prob-of-v c person))))
  (define expected-agreement/choice
    (for/list ([c expected-v])
      (apply * c)))
  (define expected-agreement (apply + expected-agreement/choice))
  (if (= 1 expected-agreement)
      #f
      (exact->inexact
       (/ (- (prob-of-agreement data) expected-agreement)
          (- 1 expected-agreement)))))

'raw-kappa
(kappa (codings->numbers complete-data coding-response-code))
'kappa-with-simplifications
(kappa (codings->numbers (simplify-data complete-data) coding-response-code))


(define (all-simplifications src# dst#)
  (define (push v lstlst) (map (// cons v <>) lstlst))
  (cond [(= src# 0) (list (list 0))]
        [(= dst# 0) (push 0 (all-simplifications (sub1 src#) 0))]
        [(= src# dst#) (push dst# (all-simplifications (sub1 src#) (sub1 dst#)))]
        [else (append
               (push dst# (all-simplifications (sub1 src#) dst#))
               (push dst# (all-simplifications (sub1 src#) (sub1 dst#))))]))

(define (all-simplifications:h src# dst#)
  (map list->hash
       (for/list ([permutation (all-simplifications src# dst#)])
         (for/list ([v (reverse permutation)] [i (in-naturals 1)]) (list i v)))))

(define (kappa-try-all/dst data src# dst#)
  (sort
   (for/list ([simplification (all-simplifications:h src# dst#)])
     (define k
       (kappa (codings->numbers (simplify-data data simplification)
                                coding-response-code)))
     (list k simplification))
   <
   #:key first))

(define (kappa-try-all data src#)
  (for/list ([i (in-range 2 src#)])
    (kappa-try-all/dst data src# i)))

'all-possible-simplifications
(kappa-try-all complete-data 6)