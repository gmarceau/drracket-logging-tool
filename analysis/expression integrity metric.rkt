#lang racket/gui

(require parser-tools/lex
         gmarceau/all
         "private/load-compiles.rkt"
         "private/common.rkt"
         "private/lab.rkt"
         "private/conversion-to-screen-player.rkt"
         unstable/function
         (planet williams/science/statistics)
         (prefix-in : parser-tools/lex-sre))

(current-test-on? #t)

(define data-dir/pro "data-processed\\data-plt-day")
(define data-dir/student "data-processed\\CS1101 - C10")

;------

(define lex
  (lexer [(eof) empty]
         [(:: #\; (:* (:~ #\newline)) #\newline) (lex input-port)]
         [#\" (let () (get-string-token input-port) (lex input-port))]
         ["#|" (let () (get-hashpipe-token input-port) (lex input-port))]
         [(union "[" "]" "(" ")" "{" "}") (cons lexeme (lex input-port))]
         [any-char (lex input-port)]))

(define get-string-token
  (lexer
   [(eof) (raise '(unbalanced string))]
   [(:~ #\" #\\) (get-string-token input-port)]
   [(:: #\\ any-char) (get-string-token input-port)]
   [#\" (void)]))

(define get-hashpipe-token
  (lexer
   [(eof) (raise '(unbalanced hashpipe))]
   ["|#" (void)]
   [any-char (get-hashpipe-token input-port)]))

(define (nesting-shape-of-str str)
  (with-handlers ([list? (lambda (_) #f)]
                  [void (lambda (exn) (pretty-print (exn-message exn)) #f)])
    (apply string-append (lex (open-input-string str)))))

(test 'nesting-shape-of-str
      (check-equal? (nesting-shape-of-str "aaa(bbb)ccc[ddd]www{bb(a)b}www") "()[]{()}")
      (check-equal? (nesting-shape-of-str "[] ;; ()\n{}") "[]{}")
      (check-equal? (nesting-shape-of-str "asd\"wqe") #f))

(define good-matches (map regexp-quote '("()" "[]" "{}")))
(define bad-matches '("(]" "(}" "[)" "[}" "{)" "{]"))

(define (A* base-case generate-subs v)
  (define memo (make-hash))
  (let loop ([v v] [prev-best +inf.0])
    (cond [(base-case v) => identity]
          [(hash-ref memo v #f) => identity]
          [(= prev-best 0) #f]
          [else
           (let ()
             (define subs (generate-subs v))
             (define-values (result found)
               (for/fold ([prev-best prev-best] [found #f]) ([sub subs])
                 (match-define (list sub-v sub-cost) sub)
                 (define result (loop sub-v (- prev-best sub-cost)))
                 (if result
                     (values (min (+ result sub-cost) prev-best) #t)
                     (values prev-best found))))
             (when found (hash-set! memo v result))
             (and found result))])))

(test 'A*
      (define (base-case v) (and (integer? v) v))
      (define (recursive-case v) (map (lambda (i) (list i 1)) v))
      (check-equal? (A* base-case recursive-case
                        '((8 9) ((1 2 (0 (0)) 3 4) 3) (((1 2 (0 (0)) 3 4) 3) 0 6)))
                    2.0))



(define (amount-of-unbalancing shape)
  
  (define (find-first-bad-match shape)
    (ormap (lambda (b) (and (regexp-match (regexp-quote b) shape) b)) bad-matches))
  
  (define (find-first-good-match shape)
    (ormap (lambda (g) (and (regexp-match? g shape) g)) good-matches))
  
  (define (base-case shape)
    (cond [(equal? shape "") 0]
          [(and (not (find-first-good-match shape)) (not (find-first-bad-match shape)))
           (string-length shape)]
          [else #f]))
  
  (define (recursive-case shape)
    (cond [(find-first-good-match shape)
           =>
           (lambda (g) (list (list (regexp-replace g shape "") 0)))]
          [(find-first-bad-match shape)
           =>
           (lambda (match)
             (for/list ([sub (list ""
                                   (string (string-ref match 0))
                                   (string (string-ref match 1)))])
               (list (regexp-replace (regexp-quote match) shape sub) 1)))]))
  
  (inexact->exact (A* base-case recursive-case shape)))

(test 'amount-of-unbalancing
      (check-equal? (amount-of-unbalancing "()[((())){{{}}}]") 0)
      (check-equal? (amount-of-unbalancing "()[))){{{}}}]") 3)
      (check-equal? (amount-of-unbalancing "([[[)((") 5)
      (check-equal? (amount-of-unbalancing "){)()[]{}[}[") 4)
      (check-equal? (amount-of-unbalancing "([)") 1)
      (check-equal? (amount-of-unbalancing "(])") 1)
      (check-equal? (amount-of-unbalancing "([)") 1)
      (check-equal? (amount-of-unbalancing "(])") 1)
      (check-equal? (amount-of-unbalancing "(][)") 2) ;; assumption here: nobody puts parentheses backward by mistake
      (check-equal? (amount-of-unbalancing "(]))") 2)
      (check-equal? (amount-of-unbalancing "([))") 1))

  
  
  
;; only count when the paren structure changes (otherwise we would be counting who has the longest identifiers)
(define (keep-only-shape-edits/shape texts)
  (define shapes (map nesting-shape-of-str texts))
  (define result
    (for/list ([t texts]
               [s shapes]
               [prev (cons #f shapes)]
               #:when s
               #:when (not (equal? s prev)))
      (list t s)))
  (values (map first result) (map second result)))

(define (keep-only-shape-edits texts)
  (define-values (t s) (keep-only-shape-edits/shape texts))
  t)

(define (sum-of-unbalancings strs)
  (apply + (map amount-of-unbalancing (map nesting-shape-of-str strs))))

(test 'sum-of-unbalancings
      (define data '("()[((())){{{}}}]"
                     "()[))){{{}}}]"
                     "([[[)(("))
      (check-equal? (sum-of-unbalancings data) 8)
      (check-equal? (sum-of-unbalancings (keep-only-shape-edits (append data (reverse data)))) 11)
      (check-equal? (sum-of-unbalancings (keep-only-shape-edits '("((" "((" "((" "((" "(((" ")))"))) 8))

(define-struct record (timestamp operation start len content) #:prefab)

(define (insert-record edi r)
  (case (record-operation r)
    ('insert (send edi insert (record-content r) (record-start r)))
    ('on-delete (send edi delete (record-start r) (+ (record-start r) (record-len r))))))

(define (playback-records records)
  (define edi (new text%))
  (for/list ([r records])
    (insert-record edi r)
    (send edi get-text)))

(define (merge-auto-events max-time items)
  (let loop ([items items])
    (match items
      [(list) empty]
      [(list (and item1 (hash-table ['event 'insert] ['milliseconds time1] ['start s1] ['len len1] ['text (list text1)] [_ _] ...))
             (and item2 (hash-table ['event 'insert] ['milliseconds time2] ['start s2] ['len len2] ['text (list text2)] [_ _] ...))
             rst ...)
       (if (and (= s2 (+ s1 len1))
                (< (- time2 time1) max-time))
           (loop (cons (hash-set-all item1
                                     'start s1
                                     'milliseconds time2
                                     'len (+ len1 len2)
                                     'text (list (string-append text1 text2)))
                       (drop items 2)))
           (cons item1 (loop (rest items))))]
      [(list item1 rst ...) (cons item1 (loop rst))])))

(define (remove-meta-opens items)
  (let loop ([items items])
    (match items
      [(list) empty]
      [(list (and item1 (record time1 'insert position1 1 ")"))
             (and item2 (record time2 'insert position2 1 "("))
             rst ...)
       (if (and (= position1 position2)
                (<= (- time2 time1) 1))
           (cons (record time1 'insert position1 2 "()") (loop rst))
           (cons item1 (loop (rest items))))]
      [(list item1 rst ...) (cons item1 (loop rst))])))
#;
(test 'remove-meta-opens
      (define data
        (list
         (record 0 0 0 0 0)
         (record 0 'insert 15 1 ")")
         (record 0 'insert 15 1 "(")
         (record 1 1 1 1 1)
         (record 0 'insert 15 1 ")")
         (record 0 'insert 15 1 "(")
         (record 0 'insert 15 1 ")")
         (record 2000 'insert 15 1 "(")
         (record 2 2 2 2 2)))
      (check-match (remove-meta-opens data)
                   (list (record 0 0 0 0 0) (record 1 1 1 1 1) (record 0 'insert 15 1 ")") (record 2000 'insert 15 1 "(") (record 2 2 2 2 2))))


(define (assemble-texts compile)
  (define items (map list->hash (with-input-from-file (.. compile 'filenames 'rktdata) (lambda () (read-all)))))
  (playback-records (remove-meta-opens (convert-items items))))

(define (assemble-compile compile)
  (define-values (texts shapes) (keep-only-shape-edits/shape (assemble-texts compile)))
  (define unbalancings (map amount-of-unbalancing shapes))
  (item-join
   (-- compile 'filenames)
   (hash
    ;     'texts texts
    'shapes shapes
    'unbalancings unbalancings
    'metric (apply + unbalancings))))

(define (all-pairwise-different? lst)
  (for/and ([i lst]
            [prev (cons #f lst)])
    (not (equal? i prev))))



(define (assemble-compiles compiles)
  (define todo (length compiles))
  (define (assemble-compile/print c)
    (pretty-print (list todo (item-select c 'username 'tab 'compile)))
    (set! todo (sub1 todo))
    (assemble-compile c))
  (hash-map-values:h (table-group-by compiles 'username) (// map assemble-compile/print <>)))

(define (summarize users)
  (hash-map-values:h users (// select <> #:fields '(unbalancings metric))))


(begin
  (define pro-users (assemble-compiles (load-compiles data-dir/pro)))
  (define stu-users (assemble-compiles (load-compiles data-dir/student)))
  
  (with-saved-output
   "expression integrity metric" #:extension "rktdata"
   (lambda ()
     (pretty-print (hash 'pros pro-users 'students stu-users))))
  
  )