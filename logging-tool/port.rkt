#lang scheme/gui

(require "snip.rkt")

(provide (all-defined-out))

#;
(define (save-file-of-port scratch-editor port)
  (define output (open-output-bytes))
  (send scratch-editor erase)
  (let loop ()
    (define v (read-char-or-special port))
    (unless (eof-object? v)
      (send scratch-editor insert v)
      (loop)))
  (send scratch-editor save-port output)
  (send scratch-editor erase)
  (get-output-bytes output))

(define (save-file-of-port port)
  (let loop ([acc empty])
    (define (consume-acc v)
      (list* (list->bytes (reverse acc)) v (loop empty)))
    
    (define v (read-byte-or-special port))
    (cond
      [(eof-object? v) (list (list->bytes (reverse acc)))]
      [(v . is-a? . snip%)
       (consume-acc (marshal-snip v))]
      [(byte? v) (loop (cons v acc))]
      [else (consume-acc v)])))

(define (duplicate-port port)
  (define-values (in1 out1) (make-pipe-with-specials #f (object-name port) (object-name port)))
  (define-values (in2 out2) (make-pipe-with-specials #f (object-name port) (object-name port)))
  (define-values (line col pos) (port-next-location port))
  (for-each port-count-lines! `(,in1 ,in2 ,out1 ,out2))
  (define in1loc (relocate-input-port in1 line col pos))
  (define in2loc (relocate-input-port in2 line col pos))
  (for-each port-count-lines! `(,in1loc ,in2loc))
  (let loop ()
    (define v (read-byte-or-special port))
    (unless (eof-object? v)
      (if (byte? v)
          (begin (write-byte v out1)
                 (write-byte v out2))
          (begin (write-special v out1)
                 (write-special v out2)))
      (loop)))
  (close-output-port out1)
  (close-output-port out2)
  (values in1loc in2loc))

#|
(define (show-loc p)
  (call-with-values (lambda () (port-next-location p)) (lambda vs (printf "~a~n" vs))))
  
(define p (open-input-string "1 2 3 4"))
(port-count-lines! p)
(read p)
(show-loc p)
(define-values (p1 p2) (duplicate-port p))
(show-loc p1)
(show-loc p2)
|#