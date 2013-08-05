#lang racket

(require parser-tools/lex
         gmarceau/all
         "private/load-compiles.rkt"
         "private/common.rkt"
         "private/lab.rkt"
         unstable/function
         (planet williams/science/statistics)
         (prefix-in : parser-tools/lex-sre))

(define data-dir/pro "data-plt-day")


(define (find-meta-opens max-time items)
  (let loop ([items items])
    (match items
      [(list) empty]
      [(list (and item1 (hash-table ('event 'insert) ('text (list ")")) ('milliseconds time1) (_ _) ...))
             (and item2 (hash-table ('event 'set-position) (_ _) ...))
             (and item3 (hash-table ('event 'insert) ('text (list "(")) ('milliseconds time2) (_ _) ...))
             rst ...)
       (if (< (- time2 time1) max-time)
           (cons (list item1 item2 item3) (loop rst))
           (loop (rest items)))]
      [_ (loop (rest items))])))

(current-test-on? #t)
(test 'find-meta-opens
      (define data
        (map list->hash
             '(((event insert)
                (start 15)
                (len 1)
                (text (")"))
                (time #s(struct:date 11 17 12 29 1 2011 6 28 #f -18000))
                (milliseconds -758692330)
                (identity 043C5FB5-E515-4065-BB77-0C03098EDAF3)
                (hostname "latitude-laptop.student.admin.wpi.edu")
                (username "gmarceau"))
               ((event set-position)
                (start 15)
                (end 15)
                (time #s(struct:date 11 17 12 29 1 2011 6 28 #f -18000))
                (milliseconds -758692314)
                (identity 043C5FB5-E515-4065-BB77-0C03098EDAF3)
                (hostname "latitude-laptop.student.admin.wpi.edu")
                (username "gmarceau"))
               ((event insert)
                (start 15)
                (len 1)
                (text ("("))
                (time #s(struct:date 11 17 12 29 1 2011 6 28 #f -18000))
                (milliseconds -758692314)
                (identity 043C5FB5-E515-4065-BB77-0C03098EDAF3)
                (hostname "latitude-laptop.student.admin.wpi.edu")
                (username "gmarceau")))))
      (check-match (find-meta-opens 50 data)
                   (list (list a b c))))


(define compiles (load-compiles data-dir/pro))
(define meta-opens (append* (map (lambda-pipe (items-of-compile <>) (find-meta-opens 100 <>)) compiles)))
(length meta-opens)

