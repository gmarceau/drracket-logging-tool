#lang scheme

(require framework/test
         scheme/gui/base)

(define ev (make-eventspace))

(define f (parameterize ([current-eventspace ev])
            (new frame% [label ""])))

(define e (new editor-canvas% [parent f]
               [editor (new text%)]))

(send f show #t)

(printf "before~n")

(parameterize ([current-eventspace ev])
  (for ([x (in-range 25)])
    (test:keystroke #\a)))

(printf "after~n")