#lang scheme

(require gmarceau/file
         gmarceau/hash
         racket/date
         racket/generator
         "private/loading.rkt"
         "private/item.rkt"
         "private/load-compiles.rkt"
         "private/common.rkt"
         "private/conversion-to-screen-player.rkt")


(define src-dirname (make-target-dirname "data--cs1101-c10--lab1" 2))
(define target-dir (make-target-dirname "data--cs1101-c10--lab1" 3))

;; ---------

(provide/contract [process-03 (path-string? path-string? . -> . any)])
(define (process-03 src-dirname target-dir)
  (define filenames (directory-list* src-dirname))
  
  (ensure-empty-dir target-dir)
  
  (for ([f filenames])
    (define target-filename
      (build-path target-dir
                  (format "~a.screenplayer" (path-filename f))))
    
    (convert-file f target-filename
                  #:with-category-in-filename #t
                  #:with-beginning #t
                  #:with-end #t)
    
    (copy-file f (build-path target-dir (format "~a.rktdata" (path-filename f)))))
  
  (for ([c (load-compiles target-dir)])
    (define first-log-entry ((sequence->generator (in-log-items (.. (.. c 'filenames) 'rktdata)))))
    (define seconds (date->seconds (.. first-log-entry 'time)))
    (for ([f (hash-values (.. c 'filenames))])
      (file-or-directory-modify-seconds f seconds))))

(define (do) (process-03 src-dirname target-dir))



