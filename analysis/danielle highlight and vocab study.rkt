#lang racket
(define (mean vs) (/ (apply + vs) (length vs)))
(define chi-squared-p void)

(require (planet neil/csv)
         (only-in srfi/1 span)
         (only-in mzlib/etc identity)
         slideshow/pict
         ;(planet williams/science/statistics)
         gmarceau/all
         ;"private/stats.rkt"
         "private/pic.rkt"
         "danielle_highlight_and_vocab_study_-_answers.rkt"
         )



;; difficulty of question
;; difficulty of question vs difficulty of word

(define data-dir 
  #<<##
C:\Documents\Dropbox\error message research - shared\Experiments\2010.09 - Danielle's Highlight-vocab MQP\data
##
)
  

;; These bounds are inclusive:
(define prompt-data/raw '(("no prompt" ((1 9) (28 35) (52 61) (111 121) (144 154) (188 198)))
                          ("vocab" ((10 17) (36 44) (62 70) (100 110) (155 176)))
                          ("highlight" ((18 27) (45 51) (71 79) (122 143) (177 187)))))



;;----

(define prompt-data
  (list->hash (append* (for*/list ([d prompt-data/raw]
                                   [range (second d)])
                         (for/list ([i (in-range (first range) (add1 (second range)))])
                           (list i (first d)))))))


(current-test-on? #t)

(define (list-ref/safe lst i [default #f])
  (if (<= (length lst) i)
      default
      (list-ref lst i)))

; ------
; PARSING OF THE TABLES

(define (process-options options answers)
  (transpose
   (for/list ([opt options]
              [ans answers]
              #:when (not (match? opt (or "" (regexp "any highlighted")))))
     (list opt (not (equal? ans ""))))))

(define (process-WH-row quiz question header row)
  (match-define (list options answers) (process-options (rest header) (rest row)))
  (define id (string->number (first row)))
  (define answer-codes (map second (.. (find quiz-questions 'name (list quiz question 1)) 'answer-codes)))
  (hash 'id id
        'type 'WH
        'prompt (hash-ref prompt-data id #f)
        'comment (list-ref/safe row (add1 (length options)) "")
        'quiz quiz
        'question question
        'subtable# 1
        'options options
        'answer-codes answer-codes
        'answers answers))

(define (is-WH-data-row? row)
  (and (string->number (first row)) #t))

(define (separate-WR-subrows lengths row)
  (if (empty? lengths)
      empty
      (let ()
        (define-values (hd rst) (split-at row (first lengths)))
        (cons hd (separate-WR-subrows (rest lengths) rst)))))

(test (check-equal? (separate-WR-subrows '(2 3) '(a b c d e))
                    '((a b) (c d e))))

(define (separate-WR-subtables lengths data)
  (transpose (map (// separate-WR-subrows lengths <>) data)))

(test (check-equal? (separate-WR-subtables '(2 3) '((a b c d e) (1 2 3 4 5)))
                    '(((a b) (1 2)) ((c d e) (3 4 5)))))


(define (WR-subtable-lengths keywords)
  (define-values (hd rst) (span (// equal? "" <>) (rest keywords)))
  (if (empty? rst)
      (list (add1 (length hd)))
      (cons (add1 (length hd)) (WR-subtable-lengths rst))))
(test (check-equal? (WR-subtable-lengths '("function call:" "" "" "" "" "" "" "" "" "open paren" "" "" "" "" "" "" "" "number" "" "" "" "" "" ""))
                    '(9 8 7)))

(define (process-WR-row quiz question i k header row)
  (match-define (list options answers) (process-options (rest header) (rest row)))
  (define comment (list-ref/safe row (add1 (length options)) ""))
  (define answer-codes (map second (.. (find quiz-questions 'name (list quiz question (add1 i))) 'answer-codes)))
  (define id (string->number (first row)))
  (hash 'id id
        'prompt (hash-ref prompt-data id #f)
        'type 'WR
        'quiz quiz
        'question question
        'subtable# (add1 i)
        'keyword k
        'options options
        'answer-codes answer-codes
        'answers answers
        'comment comment))

(define (process-WR-table quiz question data)
  (match data
    [(list keywords options rows ...)
     (define lengths (WR-subtable-lengths (first data)))
     (define keywords (filter-not (// equal? "" <>) (first data)))
     (define tables (separate-WR-subtables lengths (rest data)))
     (for/append ([(tbl i) (in-indexed tables)]
                  [k keywords])
       (for/list ([row (rest tbl)]
                  #:when (string->number (first row)))
         (process-WR-row quiz question i k (first tbl) row)))]))

(define (table-type tbl)
  (match tbl
    [(list (list (regexp "Quiz ID") _ ...) _ ...) 'WHighlight]
    [(list _ (list (regexp "Quiz ID") _ ...) _ ...) 'WReferent]))

(define (process-WH-table quiz question data)
  (for/list ([row (filter is-WH-data-row? data)]) (process-WH-row quiz question (first data) row)))

(define (process quiz question data)
  (match (table-type data)
    ['WHighlight (process-WH-table quiz question data)]
    ['WReferent (process-WR-table quiz question data)]))


; ------
; STUDENT SCORE

(define dont-reduce (make-parameter '(correct)))

(define (reduce-code code)
  (define dont-reduce:s (apply set (dont-reduce)))
  (or (and (set-member? dont-reduce:s code) code)
      (and (list? code) (findf (// set-member? dont-reduce:s <>) code))
      'wrong))
(test 'reduce-code
      (check-equal? (reduce-code 'correct) 'correct)
      (check-equal? (parameterize ([dont-reduce '(correct expected)]) (reduce-code '(a expected))) 'expected)
      (check-equal? (reduce-code 'xx) 'wrong))

(define (reduced-answer-code item)
  (define rs (for/list ([a (.. item 'answers)]
                        [ac (.. item 'answer-codes)]
                        #:when a)
               (reduce-code ac)))
  (or (for/first ([c '(wrong expected correct)] #:when (member c rs)) c)
      'wrong))

(define (answer-code item)
  (define rs (for/list ([a (.. item 'answers)]
                        [ac (.. item 'answer-codes)]
                        #:when a)
               ac))
  (match rs
    [(list) #f #;(error 'answer-code "no answer given: ~a" item)]
    [(list c) c]
    [_ #f #;(error 'answer-code "more than one answer given: ~a:" item)]))
    

(define (average-score items)
  (pipe (map reduced-answer-code items)
        (count-instances <>)
        (normalize-counts <>)
        (counts->hash <>)
        (.. <> 'correct #:default 0)))

;----

(define data (pipe (for/append ([f (directory-list data-dir)])
                     (match-define (list _ quiz question) (regexp-match "q(.)q(.).csv" f))
                     (process (string->number quiz) (string->number question) (call-with-input-file (build-path data-dir f) csv->list)))
                   (table-add-column <> 'reduced reduced-answer-code)
                   (table-add-column <> 'name quiz-question-name)))

;; sanity check
(for ([q (select data 'type 'WR)])
  (define from-data (.. q 'options))
  (define from-rubric (map first (.. (find quiz-questions 'name (quiz-question-name q)) 'answer-codes)))
  (unless (equal? from-data from-rubric)
    (define missing (remove* from-rubric from-data))
    (error 'sanity-check "WR answers from data doesn't match those from the rubric~nquestion: ~a~nfrom data:   ~s~nfrom rubric: ~s~nmissing:     ~s~n" 
           (quiz-question-name q) from-data from-rubric missing)))


(define students (for/list ([i (unique (column data 'id))])
                   (hash 'id i 
                         'score (average-score (select data 'id i))
                         'final-question (for/first ([fq final-question] #:when (= (first fq) i)) (rest fq))
                         'prompt (.. prompt-data i))))


; ------
; SUMMARIZING THE DATA

(define (percentages lst)
  (define total (apply + lst))
  (map (lambda-pipe (/ <> total) (* <> 1000) (->int <>) (/ <> 10)) lst))

(define (summary table)
  (define options
    (match (unique (select table #:field 'options))
      [(list options) options]
      [_ (error 'table "not all items have the same options")]))
  
  (define totals
    (map (// apply + <>)
         (transpose
          (for/list ([i table])
            (define c (count identity (.. i 'answers)))
            (map (match-lambda [#f 0] [#t (/ 1 c)]) (.. i 'answers))))))
  
  (for/list ([opt options]
             [code (.. (first table) 'answer-codes)]
             [p (percentages totals)])
    `((,code ,opt) ,p)))


; -----
; ENTROPY

(define (entropy option-weights)
  (mean (for/list ([w option-weights]) (if (= w 0) 0 (* w (log w))))))

(define (table-entropy table) (entropy (map second (summary table))))

; -----
; HISTOGRAMS IMAGES


;; The label is (or/c (cons/c code any) any)
;; If a code is given, it is passed to color-code-fn to obtain a color
(define (histogram header label/value-lst
                   #:color-code-fn [color-code-fn #f]
                   #:width [width 50]
                   #:max [given-max #f])
  (define line-height 11)
  (define mx (or given-max (apply max (map second label/value-lst))))
  (define (w v) (* (/ v mx) width))
  (define (pick-color label)
    (if color-code-fn
        (match (color-code-fn label)
          [(and (list color new-label) v) v]
          [(and (? string?) v) (list v label)])
        (list "black" label)))
  (define lines (apply map (lambda (label v)
                             (match-define (list color new-label) (pick-color label))
                             (hc-append 10
                                        (text (->string new-label) 'default line-height)
                                        (lt-superimpose
                                         (colorize (filled-rectangle width line-height) "light blue")
                                         (colorize (filled-rectangle (w v) line-height) color))
                                        (lt-superimpose (text (->string (inexact->exact (floor v))) 'default line-height)
                                                        (blank 100 line-height))
                                        ))
                       (transpose label/value-lst)))
  (vc-append 15 (text (->string header) 'default line-height) (apply vr-append lines)))

(define (answer-code->color code)
  (match code
    [(list 'correct v) (list "blue" v)]
    [(list 'expected v) (list "orange" v)]
    [(list _ v) (list "black" v)]))

(define (row-of-histograms label/value-lstlst
                           #:color-code-fn [color-code-fn #f]
                           #:width [width 50]
                           #:max [given-max #f])
  (apply ht-append 30
         (apply map (// histogram <> <> #:color-code-fn color-code-fn
                        #:width width
                        #:max given-max)
                (transpose (sort label/value-lstlst string<? #:key (compose ->string first))))))

(define (square-of-histograms label/value-lstlstlst
                              #:color-code-fn [color-code-fn #f]
                              #:width [width 50]
                              #:max [given-max #f])
  (apply vl-append 30
         (map (// row-of-histograms <>
                  #:color-code-fn color-code-fn
                  #:width width
                  #:max given-max)
              (sort label/value-lstlstlst string<? #:key (compose ->string first first)))))






; --------
; ANSWER CODES

(define (answer-code-contributions item)
  (define c (count identity (.. item 'answers)))
  (for/list ([code (.. item 'answer-codes)]
             [answer (.. item 'answers)]
             #:when answer)
    (list code (/ 1 c))))

(define all-prompts (unique (hash-values prompt-data)))

(define (kind-of-answer-chart data #:as-percentages [as-percentages #f])
  
  (define all-codes
    (map first
         (sort (hash->list
                (count-contributions
                 (append-map answer-code-contributions data)))
               > #:key second)))
  
  (define per-prompt
    (for/list ([p all-prompts])
      (count-contributions (append-map answer-code-contributions (select data 'prompt p)))))
  
  (define (maybe-percentages lst)
    (if as-percentages (percentages lst) lst))
  
  (list->hash
   (for/list ([p all-prompts]
              [r per-prompt])
     (list p
           (map list
                all-codes
                (maybe-percentages (map (// .. r <> #:default 0) all-codes)))))))




(define (reduce-counts lstlst)
  (define reduced (map (match-lambda [(list code v) (list (reduce-code code) v)]) lstlst))
  (hash-map-values (group-by:h reduced first #:map second) (// apply + <>)))

(define (count/reduce tbl)
  (hash-map-values:h (table-group-by tbl 'prompt)
                     (lambda-pipe (map reduced-answer-code <>) (count-instances:h <>))))

(define (table->square table)
  (define keys (hash-keys (first table)))
  (for/list ([i table]) (for/list ([k keys]) (.. i k #:default 0))))

(define (square:h->square hh)
  (table->square (hash-values hh)))

(define (hash-keep h . keys)
  (for/fold ([result empty-hash]) ([k keys]) (!! result k (.. h k))))

(define (chi-squared-p:h hh) (chi-squared-p (square:h->square hh)))

; -------
; RESULTS
#;
(define quiz-question-name-prompt
  (let ()
    (define d (table-group-by data 'name))
    
    (define (make-row qqs items)
      (match-define (list quiz question sub) qqs)
      (define keyword (match items [(list (hash-table ['keyword k] [_ _] ...) _ ...) (list '-- k)] [_ empty]))
      `((quiz ,quiz question ,question ,p ,@keyword) ,(summary items)))
    
    (filter-not empty? (apply map make-row (transpose d)))))
#;
(let ()
  (define histogram-pict (square-of-histograms quiz-question-name-prompt #:color-code-fn answer-code->color))
  (save-to-png histogram-pict "out/danielle highlight and vocab study.png")
  histogram-pict)


(define reduced-count (count/reduce (select data 'type 'WH)))
(chi-squared-p:h (hash-keep reduced-count "vocab" "highlight"))
(chi-squared-p:h (hash-keep reduced-count "vocab" "no prompt"))
(chi-squared-p:h (hash-keep (count/reduce (select data 'type 'WR)) "highlight" "no prompt"))


(row-of-histograms (hash->list (kind-of-answer-chart (select data 'type 'WH) #:as-percentages #t))
                   #:width 100 #:max 100)


(row-of-histograms (hash->list (kind-of-answer-chart (select data 'type 'WR) #:as-percentages #t))
                   #:width 100 #:max 100)

#;
(for/list ([b '(low mid high)])
  (list b 
        (row-of-histograms (hash->list (kind-of-answer-chart (select data 'student-bucket b 'type 'WH) #:as-percentages #t))
                           #:width 100 #:max 100)))

#;
(for/list ([b '(low mid high)])
  (list b 
        (row-of-histograms (hash->list (kind-of-answer-chart (select data 'student-bucket b 'type 'WR) #:as-percentages #t))
                           #:width 100 #:max 100)))

#;
(for/list ([b '(low mid high)])
    (define counted (count/reduce (select data 'student-bucket b 'type 'WH)))
    (list b counted
          (chi-squared-p:h (hash-keep counted "vocab" "no prompt"))
          (chi-squared-p:h (hash-keep counted "highlight" "no prompt"))))
#;
(for/list ([b '(low mid high)])
    (define counted (count/reduce (select data 'student-bucket b 'type 'WR)))
    (list b counted
          (chi-squared-p:h (hash-keep counted "vocab" "no prompt"))
          (chi-squared-p:h (hash-keep counted "highlight" "no prompt"))))

;------

(require mrmathematica/main)

(define Math MathEval)
(define Math/i Mexp->image)

(define d-wr
  (for/list ([p all-prompts])
    (sort (map average-score (hash-values (table-group-by (select data 'type 'WR 'prompt p) 'id))) <)))

#;
(Math/i  `(ListLinePlot #(,@(for/list ([dd d-wr])
                              `(BinCounts ,(list->vector dd) 0.05)))))

;-----------
;
(let ()
  ;; vocab vs quiz score
  (define question-scores (hash-map-values:h (group-by:h data quiz-question-name) average-score))
  (define d (table-add-column quiz-questions 'score (lambda (i) (.. question-scores (quiz-question-name i)))))
  (define word-score-vs-quiz-score (append* (for/list ([dd (select d 'type 'WR)]) (for/list ([w (.. dd 'keywords)]) (list (second w) (.. dd 'score))))))
  (hash-map-values (group-by:h word-score-vs-quiz-score first #:map second) mean))


(define (quiz-has-expected i)
  (ormap (match?-lambda (list _ 'expected)) (.. (find quiz-questions 'name (quiz-question-name i)) 'answer-codes)))

(define d (filter quiz-has-expected data))
(dont-reduce '(correct expected))

(define students/e
  (pipe (table-group-by (table-add-column d 'reduced reduced-answer-code) 'id)
        (hash-map-values:h <> (lambda-pipe (column <> 'reduced) (count-instances:h <>)))
        (hash-map <> (lambda (id c) (!! (find students 'id id)
                                        'expected-ratio
                                        (with-handlers ([void (lambda (_) 0)])
                                          (/ (.. c 'expected #:default 0)
                                             (+ (.. c 'expected #:default 0)
                                                (.. c 'wrong #:default 0)))))))))


(define (histogram-data-for-items items)
  (pipe items
        (map answer-code-contributions <>)
        (append* <>)
        (count-contributions <>)
        (hash->list <>)
        (sort <> > #:key second)))

(define (row-of-histogram-data-for-hash hash)
  (hash-map-values hash histogram-data-for-items))

(pipe (select data 'type 'WR)
      (join-on <> quiz-questions 'name #:duplicate 'left)
      (table-group-by <> 'cat)
      (row-of-histogram-data-for-hash <>)
      (row-of-histograms <> #:width 100))

(pipe (select data 'type 'WH)
      (join-on <> quiz-questions 'name #:duplicate 'left)
      (table-group-by <> 'cat)
      (row-of-histogram-data-for-hash <>)
      (row-of-histograms <> #:width 100))



(define questions-that-have-X-as-an-answer
  (select quiz-questions 'answer-codes (// ormap (match?-lambda (list "X-ed out" 'correct)) <>) #:field 'name))


 (define (do qss)
    (for/list ([qs qss])
      (histogram (->string qs) (summary (append* (for/list ([q qs]) (select data 'name q))))
                 #:color-code-fn answer-code->color)))

(for ([(p i) (in-indexed  (do '(((1 2 1)) ((1 1 1)) ((3 1 1) (2 1 1)) ((3 2 1) (2 2 1)))))])
  (save-to-svg p (format "out/WH questions ~a.svg" i)))

(pretty-print-columns 80)
(pretty-print 
 (hash-map-values (table-group-by (join-on (select data 'type 'WR)
                                           (select quiz-questions #:fields '(name cat has-expected)) 'name) 'cat 'has-expected)
                  (lambda-pipe (map answer-code <>) (count-instances <>))))
 
(count-instances (map answer-code (select data 'type 'WR)))

