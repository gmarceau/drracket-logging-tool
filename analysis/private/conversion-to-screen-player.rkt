#lang scheme

(require "item.rkt"
         "playback.rkt"
         "loading.rkt"
         "classifying.rkt"
         gmarceau/file
         gmarceau/hash
         gmarceau/debug)

(provide convert-items convert-items/port
         convert-file
         first-error-message)

(provide/contract [convertible? ((item/c 'event) . -> . boolean?)])
(define (convertible? item)
  (and (member (hash-ref item 'event) '(insert delete)) #t))

(provide/contract [convert-time (number? number? . -> . number?)])
(define (convert-time delta-time milliseconds)
  (truncate (/ (+ delta-time milliseconds) 1000)))

(define (convert-snip s)
  (cond [(bytes? s) (bytes->string/utf-8 s)]
        [(string? s) s]
        [else "."]))

(provide/contract [convert-text (list? . -> . string?)])
(define (convert-text text)
  (string-append* (map convert-snip text)))

(provide/contract [convert-item (number? (item/c 'milliseconds 'event 'start 'len 'text) . -> . record?)])
(define (convert-item delta-time item)
  (make-record
   (convert-time delta-time (.. item 'milliseconds))
   (if (eq? (.. item 'event) 'insert) 'insert 'on-delete)
   (.. item 'start)
   (.. item 'len)
   (convert-text (.. item 'text))))

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

(define max-delay-for-merge 15)


(define (convert-items items)
  (cond [(empty? items) empty]
        [else
         (define delta-time (max 0 (- (.. (first items) 'milliseconds))))
         
         (define initial
           (if (item-of-type? (first items) 'execution)
               (let ()
                 (define item (first items))
                 (define text (convert-text (.. item 'text)))
                 (list (make-record (convert-time delta-time (.. item 'milliseconds))
                                    'insert
                                    0 (string-length text) text)))
               empty))
         
         (append
          initial
          (for/list ([i (merge-auto-events max-delay-for-merge items)] #:when (convertible? i))
            (convert-item delta-time i)))]))

(provide/contract [output-converted-items (list? port? . -> . void?)])
(define (output-converted-items converted output-port)
  (for ([i converted])
    (pretty-print i output-port)
    (newline output-port)))

(define (convert-items/port items output-port)
  (define converted (convert-items items))
  (output-converted-items converted output-port))

(define (escape-for-filename str max)
  (define chars (string->list str))
  (define filtered
    (for/fold ([chars chars]) ([verboten '(#\\ #\/ #\: #\* #\? #\' #\< #\> #\| #\" #\` #\# #\newline)])
      (filter (lambda (c) (not (eq? c verboten))) chars)))
  (list->string (take filtered (min max (length filtered)))))

(define (save-text txt filename)
  (define port (open-output-file filename))
  (send txt save-port port)
  (close-output-port port))

(define (convert-file filename target-filename
                      #:with-message-in-filename [with-message-in-filename #f]
                      #:with-category-in-filename [with-category-in-filename #f]
                      #:with-beginning [with-beginning #f]
                      #:with-end [with-end #f])
  (%%v (list 'convert-file filename))
  (define dst-dirname (path-dirname target-filename))
  
  (define items (load-log filename))
  
  (when (not (empty? items))
    (let ()
      (define need-label (or with-message-in-filename
                             with-category-in-filename))
      (define label
        (and need-label
             (let ()
               (define msg (first-error-message items))
               (cond
                 [(not msg) #f]
                 [with-message-in-filename msg]
                 [with-category-in-filename
                  (classify-error-message msg #:first-cat-only #t)]))))
      
      (define filename-with-label
        (if label
            (build-path dst-dirname
                        (format "~a--~a.~a"
                                (path-basename target-filename)
                                (escape-for-filename label 120)
                                (path-extension target-filename)))
            target-filename))
      
      (define port (open-output-file filename-with-label))
      (define converted-items (convert-items items))
      (output-converted-items converted-items port)
      (close-output-port port)
      
      (when (and with-beginning
                 (item-of-type? (first items) 'execution))
        (save-text (unmarshal-text (.. (first items) 'text))
                   (build-path dst-dirname
                               (format "~a.beginning.rkt" (path-basename target-filename)))))
      
      (when with-end
        (save-text (apply-records converted-items)
                   (build-path dst-dirname
                               (format "~a.end.rkt" (path-basename target-filename)))))))
  )

