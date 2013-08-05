#lang racket
(provide post-installer)
(define (post-installer . plt-home)
  (printf "~n~nSuccess! The logging tool is installed.~n~nClick the 'close' button then quit and restart DrRacket."))
