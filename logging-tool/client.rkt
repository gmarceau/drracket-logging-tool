#lang scheme

(require "common.rkt"
         net/ssl-tcp-unit
         net/tcp-sig
         framework
         web-server/private/gzip)

(provide start-client-daemon
         client-send-available-data
         ;; provided for tests
         send-read-data-to-server/port)


(define send-thread-sleep-time (* 1 60))

(define-values/invoke-unit
  (make-ssl-tcp@ #f #f #f #f #f #f #f)
  (import)
  (export (prefix ssl: tcp^)))

;; It's possible to have two different threads trying to send the available data:
;; The client deamon thread, and the thread that is sending the data as we are exitting DrScheme.
;; See exit:insert-on-callback at the bottom of this file.
(define client-semaphore (make-semaphore 1))

;; read-log-file-data: Should be called in the context of client-semaphore
(define (read-log-file-data)
  (call-with-semaphore
   file-semaphore
   (lambda ()
     (if (file-exists? (log-filename))
         (values (file-size (log-filename))
                 (file->bytes (log-filename)))
         (values 0 #"")))))

;; trim-log-file: Should be called in the context of client-semaphore
(define (trim-log-file file-size-at-read)
  (call-with-semaphore
   file-semaphore
   (lambda ()
     (define log-input-port (open-input-file (log-filename)))
     (define temp-log-filename (format "~a.2" (log-filename)))
     (define log-copy-output-port (open-output-file temp-log-filename #:exists 'replace))
     (file-position log-input-port file-size-at-read)
     (copy-port log-input-port log-copy-output-port)
     (close-input-port log-input-port)
     (close-output-port log-copy-output-port)
     (delete-file (log-filename))
     (rename-file-or-directory temp-log-filename (log-filename) #t))))

(define (send-read-data-to-server/port data server-input-port server-output-port)
  (define gziped-data (gzip/bytes data))
  (display `(,(bytes-length gziped-data)) server-output-port)
  (display gziped-data server-output-port)
  (flush-output server-output-port)
  (match (read server-input-port)
    ;; crash if it's not 'OK, get caught by the catch-all handler then loops
    [(list 'ok) (void)])
  (close-input-port server-input-port)
  (close-output-port server-output-port))

;; send-read-data-to-server: Should be called in the context of client-semaphore
(define (send-read-data-to-server data)
  (define-values (server-input-port server-output-port)
    (ssl:tcp-connect (server-host) server-port-number))
  
  (send-read-data-to-server/port data server-input-port server-output-port))

(define (client-send-available-data)
  (call-with-semaphore
   client-semaphore
   (lambda ()
     (define-values (file-size-at-read data) (read-log-file-data))
     (when (> file-size-at-read 0)
       (send-read-data-to-server data)
       (trim-log-file file-size-at-read)))))

(define (client-daemon-proc)
  (with-maybe-silent-failure
   mute-client-failures?
   (lambda ()
     (let loop ()
       (client-send-available-data)
       (sleep send-thread-sleep-time)
       (loop))))
  (sleep send-thread-sleep-time)
  (client-daemon-proc))

(define (start-client-daemon)
  (ignore (thread client-daemon-proc))
  (exit:insert-on-callback 
   (lambda ()
     (with-maybe-silent-failure
      mute-client-failures?
      client-send-available-data))))

;; what if the log file doesn't exists?
;; test if server is down. exception while transferring?







