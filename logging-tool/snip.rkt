#lang scheme/gui

(provide (all-defined-out))

(define (split-snip snip n)
  (define box-a (box (make-object snip%)))
  (define box-b (box (make-object snip%)))
  (send (send snip copy) split n box-a box-b)
  (values (unbox box-a) (unbox box-b)))

(define (split-snip/left snip pos)
  (let-values ([(a b) (split-snip snip pos)]) a))

(define (split-snip/right snip pos)
  (let-values ([(a b) (split-snip snip pos)]) b))

(define (split-snip/pos editor snip pos)
  (define p (send editor get-snip-position snip))
  (split-snip snip (- pos p)))

(define (split-snip/pos/left editor snip pos)
  (let-values ([(a b) (split-snip/pos editor snip pos)]) a))

(define (split-snip/pos/right editor snip pos)
  (let-values ([(a b) (split-snip/pos editor snip pos)]) b))

(define (trim-snip-list editor start len snips)
  (define split-1
    (match snips
      [(list fst rst ...)
       (if (< (send editor get-snip-position fst) start)
           (cons (split-snip/pos/right editor fst start) rst)
           snips)]))
  (match split-1
    [(list single)
     (if (> (send single get-count) len)
         (list (split-snip/left single len))
         (list single))]
    [(list hd ... lst)
     (if (> (+ (send editor get-snip-position lst)
               (send lst get-count))
            (+ start len))
         (append hd (list (split-snip/pos/left editor lst (+ start len))))
         split-1)]))

(define (get-snips-in-range/untrimed editor start len)
  (let loop ([snip (send editor find-snip start 'after)])
    (cond
      [(not snip) empty]
      [(>= (send editor get-snip-position snip) (+ start len)) empty]
      [else (cons snip (loop (send snip next)))])))

(define (get-snips-in-range editor start len)
  (trim-snip-list
   editor start len
   (get-snips-in-range/untrimed editor start len)))

(define (marshal-snip snip)
  (if (snip . is-a? . string-snip%)
      (send snip get-text 0 (send snip get-count))
      (let ()
        (define base (new editor-stream-out-bytes-base%))
        (define port (make-object editor-stream-out% base))
        (define sc (send snip get-snipclass))
        (send snip write port)
        (list (send sc get-classname)
              (send sc get-version)
              (send base get-bytes)))))

(define (get-marshalled-snips-in-range editor start len)
  (for/list ([snip (get-snips-in-range editor start len)])
    (if (snip . is-a? . string-snip%)
        (send snip get-text 0 (send snip get-count))
        (marshal-snip snip))))