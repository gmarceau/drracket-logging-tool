#lang scheme

(require gmarceau/util
         gmarceau/file)

(provide/contract [make-target-dirname (path-string? number? . -> . path?)])
(define (make-target-dirname base process#)
  (build-path base (format "data-processed-~a" (pad 2 #\0 process#))))

(provide/contract [ensure-empty-dir (path-string? . -> . void?)])
(define (ensure-empty-dir dirname)
  (when (directory-exists? dirname)
    (delete-directory/files dirname))
  (sleep 1)
  (make-directory dirname))

(provide/contract [with-saved-output ((path-string? (-> any/c)) (#:extension string?) . ->* . any/c)])
(define (with-saved-output code-filename thunk #:extension [extension #f])
  (with-output-to-file
      (build-path "out" (format "~a.out~a"
                                (path-basename code-filename)
                                (if extension (format ".~a" extension) "")))
    #:exists 'replace
    thunk))

(provide/contract [escape-uppercases (string? . -> . string?)])
(define (escape-uppercases str)
  (regexp-replace* #rx"[_A-Z]" str (lambda (c) (format "_~a" (string-downcase c)))))