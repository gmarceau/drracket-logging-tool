#lang scheme/gui

(require scheme/generator
         "item.rkt"
         gmarceau/all)


(provide unmarshal-snip
         in-log-items
         in-log-items/no-repl)

;; parse-ss-file : bytes -> text%
(provide/contract [parse-ss-file (bytes? . -> . (is-a?/c text%))])
(define (parse-ss-file bytes)
  (define port (open-input-bytes bytes))
  (define txt (new text%))
  (port-count-lines! port)
  (send txt insert-port port)
  txt)

(define (unmarshal-snip name data)
  (send
   (send (get-the-snip-class-list) find name)
   read
   (make-object editor-stream-in%
     (make-object editor-stream-in-bytes-base% data))))

(provide/contract [unmarshal-text ((listof (or/c string? bytes? list?)) . -> . (is-a?/c text%))])
(define (unmarshal-text list-of-strings-and-snips)
  (define txt (new text%))
  (for ([string-or-snip list-of-strings-and-snips])
    (match string-or-snip
      [(? string?)
       (send txt insert string-or-snip)]
      [(? bytes?)
       (send txt insert (bytes->string/utf-8 string-or-snip))]
      [(list 'unprintable (? (// equal? <> #"#<special-comment>")))
       (send txt insert "#;(comment box)")]
      [(? list?)
       (match-define (list name v data) string-or-snip)
       (send txt insert (unmarshal-snip name data))]))
  txt)

(provide/contract [load-code ((item/c 'text 'time) . -> . (item/c 'text%))])
(define (load-code item)
  (define txt (unmarshal-text (hash-ref item 'text)))
  (send txt set-filename 
        (format "~a:~a" (and (?? item 'logfile) (.. item 'logfile))
                (hash-ref item 'time)))
  (hash-set item 'text% txt))

(define (flatten-text item)
  (define (convert-snip s)
    (cond [(bytes? s) s]
          [(string? s) (string->bytes/utf-8 s)]
          [else #"'x"]))
  
  (define (convert-text text)
    (apply bytes-append (map convert-snip text)))
  
  (hash-update item 'text convert-text))

(define (fix-text h)
  (if (list? (hash-ref h 'text (lambda () empty)))
      h
      (hash-set h 'text (list (hash-ref h 'text)))))

(define (fix-time h)
  (define d1 (hash-ref h 'time))
  (define d2 (if (vector? d1)
                 (rest (vector->list d1))
                 (rest (struct->list d1))))
  (hash-set h 'time (apply make-date d2)))

(define (fix-item item)
  (define h (make-immutable-hash (map list->dotted-pair item)))
  (fix-time (fix-text h)))

(provide/contract [load-log (path-string? . -> . (listof item?))])
(define (load-log filename)
  (for/list ([i (in-log-items filename)]) i))

(define (in-log-items log-filename)
  (in-generator
   (for ([i (in-read-file log-filename)])
     (yield (fix-item i)))))

(define (is-repl? item) (item-of-type? item 'repl))
(define (is-exn? item) (?? item 'exn-message))

(define (in-log-items/no-repl filename)
  (in-generator
   (for/fold ([after-repl? #f])
     ([item (in-log-items filename)])
     (cond [(is-repl? item) #t]
           [(and (is-exn? item) after-repl?) #t]
           [else (yield item) #f]))))



