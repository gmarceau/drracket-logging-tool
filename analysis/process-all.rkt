#lang racket

(require (except-in "process-01-move-to-one-file-per-tab.rkt" consented?)
         "process-02-per-error-message--no-repl.rkt"
         "process-03-export-to-screen-player.rkt"
         "process-04-gather-into-a-single-dir.rkt"
         gmarceau/file
         gmarceau/util
         gmarceau/debug)

(struct target (filename destination))

(define src-dirname "data\\CS1101 - A10")
(define dest-dirname "data-processed\\CS1101 - A10")


(define (consented? username) true)

;; ---------

(define rx #rx"drscheme-tool-log-entries-received-(.*)")

(define (find-logfiles dir)
  (for/list ([f (directory-list* dir)]
             #:when (not (equal? (string->path "gz") (path-extension f)))
             #:when (regexp-match rx (path->string f)))
    f))

(define (make-temporary-dir [template "mztmp~a"])
  (define filename (make-temporary-file template))
  (delete-file filename)
  (make-directory filename)
  filename)

(define (process-one log-filename target-dirname consented?)
  (match-define
   (and temp-dirs (list temp1 temp2))
   (build-list 2 (lambda-pipe (format "process-all-~a-" <>)
                              (string-append <> "-~a")
                              (make-temporary-dir <>))))
  
  (%% process-01 log-filename temp1 consented?)
  (%% process-02 temp1 temp2)
  (%% process-03 temp2 target-dirname)
  
  (for ([d temp-dirs])
    (with-handlers ([void void]) (delete-directory/files d))))

(define (process-targets targets consented?)
  (for ([t targets])
    (process-one (target-filename t) (target-destination t) consented?)))

(define (process-and-gather logfiles dest-dirname consented?)
  (define base-dir (make-temporary-dir "process-all-~a"))
  (define targets
    (for/list ([f logfiles]) (target f (build-path base-dir (second (regexp-match rx (path->string f)))))))
  (process-targets targets consented?)
  (process-04 base-dir base-dir)
  (for-each delete-directory/files (map target-destination targets)))

(define (do) (process-and-gather (find-logfiles src-dirname) dest-dirname consented?))


