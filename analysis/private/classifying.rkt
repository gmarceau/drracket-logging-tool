#lang scheme

(require gmarceau/counting
         gmarceau/hash
         "loading.rkt"
         "item.rkt")

(define error-patterns
  '((#rx"name is not defined," unbound-id)
    (#rx"reference to undefined identifier:" unbound-id)
    (#rx"expects type .* as .* argument, given" runtime type)
    (#rx"expects [0-9]+ argument.?, given" arg-count)
    (#rx"read: expected a `.' to close `.'" parens matching)
    (#rx"read: expected `.' to close preceding `.', found instead `.'" parens matching)
    (#rx"function call: expected a defined name or a primitive operation name after an open parenthesis, but found" parens meaning-of-open)
    (#rx"this procedure expects .* argument.?, here it is provided .* argument.?" arg-count)
    (#rx"reference to an identifier before its definition:" definitions ordering)
    (#rx"cond: all question results were false" runtime cond)
    (#rx"expects argument of type" runtime type)
    (#rx"this primitive operator must be applied to arguments; expected an open parenthesis before the primitive operator name" parens meaning-of-open)
    (#rx"read: unexpected `.'" parens matching)
    (#rx"this constructor expects .* argument.?, here it is provided .* argument.?" arg-count)
    (#rx"read: missing `.' to close `.' on line .*, found instead `.'" parens matching)
    (#rx"check-expect requires two expressions" arg-count)
    (#rx"cond: expected a clause with one question and one answer, but found a clause with .* part.?" positional&syntax cond)
    (#rx"define: expected only one expression for the function body" positional&syntax define)
    (#rx"define: expected only one expression after the defined name" positional&syntax define)
    (#rx"this name was defined previously and cannot be re-defined" definitions duplicate)
    (#rx"question result is not true or false" runtime type)
    (#rx"cond: expected a clause with a question and answer, but found a clause with .*part.?" positional&syntax cond) 
    (#rx"cond: expected a question--answer clause, but found" positional&syntax cond)
    (#rx"illegal use of syntax" illegal-use-of-structure-identifier)
    (#rx"this is a procedure, so it must be applied to arguments" parens meaning-of-open)
    (#rx"read: missing `.' to close preceding `.', found instead `.'" parens matching)
    (#rx"this is a selector, so it must be applied to a structure to get the field value \\(which requires using a parenthesis before the name\\)"
parens meaning-of-open)
    (#rx"this selector expects .* argument.?, here it is provided .* argument.?" arg-count)
    (#rx"cannot make .* x .* image" runtime images)
    (#rx"expected a finished expression, but found a template" template)
    (#rx"procedure application: expected procedure, given:" parens meaning-of-open)
    (#rx"read: illegal use of" read)
    (#rx"this name has a built-in meaning and cannot be re-defined" definitions duplicate)
    (#rx"define: cannot redefine name:" definitions duplicate)
    (#rx"expected .* as .* argument, given:" runtime type)
    (#rx"image.*: Expected two or more images; given" arg-count)
    (#rx"and: expected at least two expressions after `and', but found" arg-count)
    (#rx"define-struct: expected a structure type name after `define-struct', but found" positional&syntax struct)
    (#rx"expected a structure field name, found" positional&syntax struct)
    (#rx"expecting a single `module' program; no program expressions given" internal-error sandbox empty-program)
    (#rx"found a test that is not at the top level" internal-error sandbox test-at-top-level-rejected)
    (#rx"network access denied" internal-error sandbox network-denied)
    (#rx"expected a defined name or a primitive operation name after an open parenthesis, but nothing's there" parens meaning-of-open)
    (#rx"procedure of one argument expected as on-redraw argument; given " positional&syntax bigbang)
    (#rx"expected argument of type .*; given" runtime type)
    (#rx"define: found a definition that is not at the top level" positional&syntax define)
    (#rx"module: identifier already imported from a different source" module duplicate-import)
    (#rx"function definitions are not allowed in the interactions window; they must be in the definitions window" no-def-allowed-in-interaction)
    (#rx"is not a valid color name" runtime color-name)
    (#rx"cons: second argument must be of type <list>, given" runtime type)
    (#rx"define: expected a name for the function's .?... argument, but found" positional&syntax define)
    (#rx"this is a predicate, so it must be applied to arguments \\(which requires using a parenthesis before the name\\)" parens meaning-of-open)
    (#rx"expected a name after an open parenthesis," parens meaning-of-open)
    (#rx"/: division by zero" runtime div-by-zero)
    (#rx"read: expected `.' to close `.' on line .*, found instead `.'" parens matching)
    (#rx"or: expected at least two expressions after `or', but found" arg-count)
    (#rx"else: not allowed here, because this is not an immediate question in a `cond' clause" positional&syntax cond)
    (#rx"link: module mismatch, probably from old bytecode whose dependencies have changed: variable not provided \\(directly or indirectly\\) from module:"
     internal-error bytecode-version)
    (#rx"define: expected at least one argument name after the function name, but found none" positional&syntax define)
    (#rx"lib: standard-module-name-resolver: collection not found" module collect-not-found)
    (#rx"open-input-file: cannot open input file: .* \\(The system cannot find the file specified.; errno=2\\)" module file-not-found)
    (#rx"define: expected a function name, constant name, or function header for `define', but found" positional&syntax define)
    (#rx"draw-world: result of type <scene> expected, your function produced" runtime type)
    (#rx"^on-tick-event:" runtime type)
    (#rx"^on-redraw:" runtime type)
    (#rx"if: expected one question expression and two answer expressions, but found" positional&syntax if)
    (#rx"cannot compare inexact numbers" runtime comparing-inexacts)
    (#rx"text-input-port: editor has changed since port was opened" internal-error port-has-changed)
    (#rx"result of type <scene> expected, your function produced image with pinhole at" runtime type)
    (#rx"stop-when: evaluate \\(big-bang Number Number Number World\\) first" positional&syntax bigbang)
    (#rx"user break" user-break)
    (#rx"read: bad syntax" read)
    (#rx"not: expected either true or false; given" runtime type)
    (#rx"define: expected a name for a function, but found" positional&syntax define)
    (#rx"this primitive operator must be applied to arguments; expected an open parenthesis before the operator name" parens meaning-of-open)
    (#rx"and: found a use of `and' that does not follow an open parenthesis" parens meaning-of-open)
    (#rx"or: found a use of `or' that does not follow an open parenthesis" parens meaning-of-open)
    (#rx"on-tick: primitive operator on-tick expects a defined procedure name \\(usually `handle-tick'\\) in this position" positional&syntax bigbang)
    (#rx"cond: found a use of `cond' that does not follow an open parenthesis"  parens meaning-of-open)
    (#rx"define: expected an expression for the function body, but nothing's there" positional&syntax define)
    (#rx"posn\\?: this predicate must be applied to an argument; expected an open parenthesis before the predicate name" parens meaning-of-open)
    (#rx"on-tick: primitive operator requires 1 arguments" positional&syntax bigbang)
    (#rx"cons: second argument must be of type <list or cyclic list>" runtime type)
    (#rx"error: expected a symbol and a string" runtime type)
    (#rx"set!: expected only one expression for the new value, but found" positional&syntax set!)
    (#rx"unquote: misuse of a comma or `unquote', not under a quasiquoting backquote" read quote)
    (#rx"set!: expected a defined name after `set!', but found" positional&syntax set!)
    (#rx"local variable used before its definition:" definition ordering)
    (#rx"cond: found an `else' clause that isn't the last clause in its `cond' expression" positional&syntax cond)
    (#rx"define-struct: expected a sequence of field names after the structure type name in `define-struct'"
        positional&syntax struct)
    (#rx"this predicate expects [0-9]+ argument, here it is provided" arg-count)
    (#rx"expects at least [0-9]+ arguments, given" arg-count)
    (#rx"read: expected a closing '.'" read)
    (#rx"define-struct: found a field name that was used more than once" positional&syntax struct)
    (#rx"define: expected an expression after the defined name" positional&syntax define)
    (#rx"draw-game: result of type <scene> expected, your function produced" runtime type)
    (#rx"define-struct: expected nothing after the field name sequence in `define-struct'" positional&syntax
        struct)
    (#rx"this selector must be applied to a structure to get the field value; expected an open parenthesis before the selector name" parens meaning-of-open)
    (#rx"require: a module-naming string can contain only a-z, A-Z, 0-9, -, _, ., space, and slash" positional&syntax
        require)
    (#rx"expects string of length 1 as first argument" runtime type)
    (#rx"expects string as second argument" runtime type)
    (#rx"require: namespace mismatch; reference \\(phase 0\\) to a module" internal-error namespace-mismatch)
    (#rx"last argument must be of type <list>, given" runtime type)
    (#rx"this is a constructor, so it must be called with values for the structure fields \\(which requires using a parenthesis before the name\\)" parens meaning-of-open)
    (#rx"quote: found a use of `quote' that does not follow an open parenthesis" read quote)
    (#rx"check-expect cannot compare functions." runtime type)
    (#rx"last argument must be of type <list or cyclic list>," runtime type)
    (#rx"cond: expected a question--answer clause after `cond', but nothing's there" positional&syntax cond)
    (#rx"define: found a use of `define' that does not follow an open parenthesis" parens meaning-of-open)
    (#rx"local: expected only definitions within the definition sequence, but found" positional&syntax local)
    (#rx"set!: found a use of `set!' that does not follow an open parenthesis" parens meaning-of-open)
    

    
    ))

(provide/contract [first-error-message (sequence? . -> . string?)])
(define (first-error-message items)
  (for/first ([i items]
              #:when (hash-has-key? i 'exn-message))
    (.. i 'exn-message)))

(define classification/c (listof (cons/c regexp? (listof symbol?))))
(define (classifier/c in/c)
  (([in in/c]) (#:first-cat-only (first-cat-only boolean?))
               . ->d . [result (or/c #f (if (or (unsupplied-arg? first-cat-only) (not first-cat-only))
                                            classification/c (listof symbol?)))]))

(provide/contract [classify-file (classifier/c path-string?)])
(define (classify-file filename #:first-cat-only [first-cat-only #f])
  (define fst-err (first-error-message (in-log-items filename)))
  (and fst-err (classify-error-message fst-err #:first-cat-only first-cat-only)))

(provide/contract [classify-item (classifier/c (item/c 'message))])
(define (classify-item item #:first-cat-only [first-cat-only #f])
  (classify-error-message (.. item 'message) #:first-cat-only first-cat-only))

(provide/contract [classify-error-message (classifier/c string?)])
(define (classify-error-message msg #:first-cat-only [first-cat-only #f])
  (define result
    (filter (match-lambda [(and lst (list rx categories ...))
                           (and (regexp-match rx msg) lst)]
                          [_ #f])
            error-patterns))
  (if first-cat-only
      (match result
        [(list (list rx cats ...)) cats]
        [(list) #f])
      result))

(provide error-message-histogram)
(define (error-message-histogram all-error-messages [categorization-depth #f])
  (define matches
    (for/list ([msg all-error-messages])
      (list msg (classify-error-message msg))))
  
  (define not-matching (filter-map (match-lambda [(list msg (list)) msg] [_ #f]) matches))
  (define single-match (filter-map (match-lambda [(list msg (list rx)) (list msg rx)] [_ #f]) matches))
  (define multiple-matches (filter-map (match-lambda [(and (list msg (list rx1 rx2 rxs ...)) lst) lst] [_ #f]) matches))
  
  (define (trim-cat categories)
    (if categorization-depth
        (take categories (min categorization-depth (length categories)))
        categories))
  
  (define error-counts
    (count-instances 
     (map (match-lambda [(list msg (list rx categories ...)) (trim-cat categories)]) single-match)))
  
  ;(unless (= 0 (length not-matching)) (raise (cons "some-didnt-match-anything" not-matching)))
  (unless (= 0 (length multiple-matches)) (raise (cons "some-matched-many-things" multiple-matches)))
  error-counts)

(define error-qualities-list
  '([0 ()]
    [1
     (parens matching)
     (read)
     (template)]
    
    [2
     (parens meaning-of-open)
     (positional&syntax cond)
     (positional&syntax struct)
     (positional&syntax define)
     (illegal-use-of-structure-identifier)]
    
    [3
     (unbound-id)
     (definitions duplicate)
     (definitions ordering)
     (arg-count)]
    
    [4
     (runtime comparing-inexacts)
     (runtime type)
     (runtime cond)]))

(provide all-error-categories)
(define all-error-categories (remove-duplicates (map rest error-patterns)))

(define error-qualities
  (for*/fold ([result empty-hash])
    ([lst error-qualities-list]
     [err (rest lst)])
    (hash-set result err (first lst))))

(provide/contract [error-quality ((or/c #f string?) . -> . integer?)])
(define (error-quality msg)
  (if (not msg)
      5
      (let ([c (classify-error-message msg)])
        (hash-ref error-qualities (if (empty? c) c (rest (first c)))))))




