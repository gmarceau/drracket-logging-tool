#lang scheme

(require gmarceau/hash
         gmarceau/cut
         gmarceau/file
         gmarceau/list
         gmarceau/debug
         gmarceau/counting
         "private/loading.rkt"
         "private/classifying.rkt"
         "private/common.rkt"
         "private/conversion-to-screen-player.rkt")

(provide subsample-errors subsample-errors/filename)

(define src-dirname (make-target-dirname "data--cs1101-c10--lab1" 2))
(define previously-used-files-dirname (list "shuffle-01" "shuffle-02"))
(define dst-dirname "shuffle-03")

(define percentile-desired 80) 
(define total-desired 100) ;; number of items desired

;;--------

(define (subsample-errors src-dirname previously-used-files-dirname dst-dirname percentile-desired total-desired)
  (define filenames (directory-list* src-dirname))
  
  (define previously-used-filenames
    (list->hash-set
     (for/list ([item (append* (map directory-list*/recursive previously-used-files-dirname))])
       (and (file-exists? item)
            (path-basename item)))))
  
  (subsample-errors/filename filenames previously-used-filenames dst-dirname percentile-desired total-desired))

(define (subsample-errors/filename filenames previously-used-filenames dst-dirname percentile-desired total-desired)
  
  (define filenames-not-previously-used
    (for/list
        ([f filenames]
         #:when
         (not (hash-has-key? previously-used-filenames (path-basename f))))
      f))
  
  (define categorized
    (for/fold ([result empty-hash]) ([f filenames-not-previously-used])
      (define classified (classify-file f))
      (if (not classified)
          result
          (let ()
            (match-define (list (list rx category ...))
                          classified)
            (hash-update result category (// cons f <>) empty)))))
  
  (define larger-ones
    (list->hash
     (take-top-percentile (hash->list categorized)
                          (/ percentile-desired 100)
                          #:key (compose length second)
                          #:cache-keys? #t)))
  
  (define categories-remaining (hash-keys larger-ones))
  (define per-category (ceiling (/ total-desired (length categories-remaining))))
  
  (define (take-at-random/at-most lst n)
    (take (shuffle lst) (min n (length lst))))
  
  (define selected
    (hash-map-values:h larger-ones (// take-at-random/at-most <> per-category)))
  
  (define (save-text txt filename)
    (define port (open-output-file filename))
    (send txt save-port port)
    (close-output-port port))
  
  (pretty-display (hash-map-values:h selected (// map path->string <>)))
  
  (ensure-empty-dir dst-dirname)
  
  (for ([cat (hash-keys selected)])
    (define sub-dir-name (build-path dst-dirname (format "~a" cat)))
    (define (make-filename fmt name)
      (build-path sub-dir-name (format fmt name)))
    
    ;; TODO: use the files resulting from process-03
    
    (%% make-directory sub-dir-name)
    (for ([f (hash-ref selected cat)])
      (convert-file f (make-filename "~a.screenplayer" (path-filename f))
                    #:with-message-in-filename #f
                    #:with-beginning #t
                    #:with-end #t)))
  
  (let ()
    (define port (open-output-file (build-path dst-dirname "marks.csv") #:exists 'replace))
    (define select:list
      (sort (hash->list selected) string<? #:key (lambda (i) (format "~a" (first i)))))
    (for* ([cat/fs select:list] [f (sort (second cat/fs) string<? #:key path->string)])
      (define-values (_1 name _2) (split-path f))
      (fprintf port "~a, ~a~n" (first cat/fs) (path->string name)))
    (close-output-port port)))

(define (do) (subsample-errors src-dirname previously-used-files-dirname dst-dirname percentile-desired total-desired))
