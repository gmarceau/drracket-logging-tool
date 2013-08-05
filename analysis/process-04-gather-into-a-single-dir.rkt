#lang racket

(require gmarceau/file
         gmarceau/table
         gmarceau/cut
         gmarceau/util
         gmarceau/test
         (planet untyped/unlib/match)
         "private/load-compiles.ss"
         "private/common.rkt"
         (planet dherman/memoize))

(current-test-on? false)

(define src-dirname "C:\\DOCUME~1\\gmarceau\\LOCALS~1\\Temp\\mztmp1281724536-323195995")
(define target-dir "data-2010-08-13")

(provide/contract [process-04 (path-string? path-string? . -> . any)])
(define (process-04 src-dirname target-dir)
  
  (define/memo* (directory-list*/m dir) (directory-list* dir))
  
  (when (directory-exists? target-dir)
    (for-each delete-file
              (filter file-exists? (directory-list* target-dir))))
  
  (when (not (directory-exists? target-dir))
    (make-directory target-dir))
  (sleep 1)
 
  (let ()
    
    (define items (map load-compiles (directory-list* src-dirname)))
    
    (define usernames
      (remove-duplicates (column->list (append* items) 'username)))
    
    (define (filter-for-username username)
      (map
       (// filter (// match? <> (hash-table ('username (equal? username)) (k v) ...)) <>)
       items))
    (test (check-pred (// > <> 0) (apply + (map length (filter-for-username "dmartin")))))
    
    (define (change-extension filename new)
      (build-path (or (path-dirname filename) 'same) (format "~a.~a" (path-basename filename) new)))
    (test (check-equal? (change-extension (string->path "asd/qwe.et") "gh")
                        (string->path "asd/qwe.gh"))
          (check-equal? (change-extension (string->path "qwe.et") "gh")
                        (string->path ".\\qwe.gh"))
          (check-equal? (change-extension
                         (string->path
                          "tmurray--tab-00--compile-005--define expected only one expression after the defined name label-near, but found one extra part.screenplayer")
                         "rktdata")
                        (string->path
                         ".\\tmurray--tab-00--compile-005--define expected only one expression after the defined name label-near, but found one extra part.rktdata")))
    
    
    (define (move-item src-filename dest-filename)
      (when (file-exists? src-filename)
        (copy-file src-filename dest-filename)))
    
    (define (move-items items)
      (for/fold ([dest-tab -1]) ([item items])
        (define ticked-dest-tab (if (= 0 (.. item 'compile)) (add1 dest-tab) dest-tab))
        
        (define label (or (.. item 'message) (.. item 'category)))
        
        (define new-basename
          (format "~a--tab-~a--compile-~a~a"
                  (escape-uppercases (.. item 'username))
                  (pad 2 #\0 ticked-dest-tab)
                  (pad 3 #\0 (.. item 'compile))
                  (if label (format "--~a" label) "")))
        
        (for ([(ext f) (.. item 'filenames)])
          (define src-filename (change-extension (.. item 'filename) ext))
          (define dest-filename (build-path target-dir (format "~a.~a" new-basename ext)))
          (move-item f (build-path target-dir (format "~a.~a" new-basename ext))))
        
        ticked-dest-tab))
    (for ([u usernames]) (move-items (append* (filter-for-username u))))))

(define (do) (process-04 src-dirname target-dir))
