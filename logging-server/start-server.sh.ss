#! /usr/bin/mzscheme 
#lang scheme

(require "server.ss")

(define client-data-filename (build-path (getenv "HOME") "drscheme-tool-log-entries-received"))
(define server-log-filename (build-path (getenv "HOME") "server-errors.log"))

(define server-log-port (open-output-file server-log-filename #:exists 'append))
(current-error-port server-log-port)
(current-output-port server-log-port)

(start-server (open-output-file client-data-filename #:exists 'append))

