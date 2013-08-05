#lang racket
(require (planet untyped/unlib/for)
         (planet untyped/unlib/match)
         (planet bzlib/date/plt)
         mzlib/string
         racket/generator
         "item.rkt"
         "loading.rkt"
         gmarceau/all)

(current-test-on? false)

(define (sort-filename lst) (sort lst string<=? #:key (compose string-downcase path->string)))

(define (parse-filename/no-time f)
  (define rx #rx"^(.+)--tab-([0-9]+)--compile-([0-9]+)(--([^.]*))?\\.(.*)$")
  (match (regexp-match rx (path-filename f))
    [(list all username tab compile _ msg extension)
     (define h (hash 'username username
                     'tab (string->number tab)
                     'compile (string->number compile)
                     'extension (string->symbol extension)
                     'filename f))
     (cond [(not msg) h]
           [(regexp-match-exact? #rx"\\(.*\\)" msg)
            (!! h 'category (read-from-string msg))]
           [(!! h 'message msg)])]
    [_ #f]))

(provide compile-item/c)
(define compile-item/c (item/c 'username 'tab 'compile))

(provide/contract [parse-filename (path-string? . -> . (or/c #f (extend-item/c compile-item/c 'extension 'filename)))])
(define (parse-filename f)
  (define item (parse-filename/no-time f))
  (and item (!! item 'time (seconds->date (file-or-directory-modify-seconds f)))))


(provide/contract [load-compiles (path-string? . -> . (listof (extend-item/c compile-item/c 'filenames)))])
(define (load-compiles dir)
  (define items (filter-map parse-filename (directory-list* dir)))
  (define grouped (group-by:h items (lambda (i) (map (// .. i <>) '(username tab compile)))))
  (define (gather-filenames group)
    (define h (list->hash (map (lambda (i) (list (.. i 'extension) (.. i 'filename))) group)))
    
    (pipe (first group)
          (!! <> 'filenames h)
          (-- <> 'extension)
          (-- <> 'filename)))
  
  (sort (hash-values (hash-map-values:h grouped gather-filenames))
        string<=? #:key (lambda (i) (format "~a-~a-~a"
                                            (string-downcase (.. i 'username))
                                            (.. i 'tab)
                                            (.. i 'compile)))
        #:cache-keys? true))


(provide/contract [items-of-compile ((item/c 'filenames) . -> . (listof item?))])
(define (items-of-compile compile)
  (load-log (.. compile 'filenames 'rktdata)))

(provide in-items-of-compiles)
(define (in-items-of-compiles compiles)
  (apply in-sequences
         (for/list ([c compiles])
           (in-log-items (.. c 'filenames 'rktdata)))))




