#lang scheme
(require "private/item.rkt"
         "private/conversion-to-screen-player.rkt"
         "private/classifying.rkt"
         gmarceau/util
         gmarceau/file
         gmarceau/counting
         gmarceau/cut
         gmarceau/hash
         (planet untyped/unlib/for))

(define filename "C:\\Documents\\collects\\analysis\\data\\drscheme-tool-log-entries-received-2010-01-20")

(define items (for/list ([i (in-log-items filename)]) i))

(define count-events
  (count-instances (map (// .. <> 'username) items)))


(define count-exn
  (count-instances (map (// .. <> 'username)
                        (filter (// ?? <> 'exn-message) items))))

(define count-compile
  (count-instances (map (// .. <> 'username)
                        (filter-by-type 'compile items))))

(define count-repl
  (count-instances (map (// .. <> 'username)
                        (filter-by-type 'repl items))))

