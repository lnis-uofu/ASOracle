#lang racket
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))

(provide ld-program vss gnd vdd negation integration multiplication addition subtraction eia input output design assign)
(provide dsl-expand negative add subtract multiply integrate constant variable)

;;;; DSL AST post expansion
;; operator expressions
(struct ld-program (vdd vss gnd design eia inputs outputs assigns))
(struct integrate (expr))
(struct add (left right))
(struct subtract (left right))
(struct negative (expr))
(struct multiply (left right))
(struct divide (left right))
(struct constant (scalar))
(struct variable (name))

(define (eia-lookup name) (list 1 2 3))

(define-syntax-rule (eia name)
  (define eia-series (eia-lookup name)))

(define-syntax (design stx)
  (syntax-case stx ()
    [(design name)
      (with-syntax
        ([ident (format-id #'name "design-name")])
        #'(define ident name))]))

(define-syntax (vdd stx)
  (with-syntax
    ([name (format-id stx "power-vdd")])
    (syntax-case stx ()
      [(vdd _ n) #'(define name (- n))]
      [(vdd n) #'(define name n)])))

(define-syntax (vss stx)
  (with-syntax
    ([name (format-id stx "power-vss")])
    (syntax-case stx ()
      [(vss _ n) #'(define name (- n))]
      [(vss n) #'(define name n)])))

(define-syntax (gnd stx)
  (with-syntax
    ([name (format-id stx "power-gnd")])
    (syntax-case stx ()
      [(gnd _ n) #'(define name (- n))]
      [(gnd n) #'(define name n)])))

(define-syntax-rule (assign name expr)
  (hash-set! assign-hash (string->symbol name) expr))

(define-syntax negation
  (syntax-rules (constant)
    [(_ (constant c))
     (constant (- c))]
    [(_ n ...)
     #'(negative n ...)]))

(define-syntax (integration stx)
  (syntax-case stx (negation)
    [(_ (negation x))
     #'(negation (integrate x))]
    [(_ x)
     #'(integrate x)]))

(define-syntax (division stx)
  (syntax-case stx (constant)
    [(_ (constant t) (constant u))
     #'(constant (/ t u))]))

(define-syntax (multiplication stx)
  (syntax-case stx (constant negation)
    [(_ (negation (constant x)) y)
     #'(multiply (constant x) (negation y))]
    [(_ y (negation (constant x)))
     #'(multiply (constant x) (negation y))]
    [(_ (constant t) (constant u))
     #'(constant (* t u))]
    [(_ _ (constant 0))
     #'(constant 0)]
    [(_ (constant 0) _)
     #'(constant 0)]
    [(_ t (constant x ...))
     #'(multiply (constant x ...) t)]
    ;; left side multiplication must be a constant term.
    [(_ (constant x ...) t)
     #'(multiply (constant x ...) t)]))

(define-syntax-rule (subtraction a b)
  (addition a (negation b)))

(define-syntax (addition stx)
  (syntax-case stx (constant)
    [(_ (constant t) (constant u))
     #'(constant (+ t u))]
    [(_ x ...)
     #'(add x ...)]))

(define-syntax-rule (input name)
  (append-input (string->symbol name)))

(define-syntax-rule (output name)
  (append-output (string->symbol name)))

(define-for-syntax (line-order a)
  (syntax-case a ()
    [(design _) 0]
    [(eia _) 1]
    [(vdd _) 2]
    [(vss _) 3]
    [(gnd _) 4]
    [(input _) 5]
    [(output _) 6]
    [(assign _) 7]))

(define-for-syntax (line-cmp a b)
  (< (line-order a) (line-order b)))

(define-syntax (dsl-expand stx)
  (syntax-case stx ()
    [(dsl-expand (linear line ...))
     (let ([sorted (syntax (sort (syntax->list line ...) line-cmp))])
       (with-syntax ([(lines ...) sorted])
         #'(#%module-begin
            (define input-list empty)
            (define (append-input item) (set! input-list (cons item input-list)))
            (define output-list empty)
            (define (append-output item) (set! output-list (cons item output-list)))
            (define assign-hash (make-hash))
            lines ...
            (define ld-system (ld-program
                               power-vdd power-vss power-gnd
                               design-name eia-series
                               (vector->list input-list)
                               (vector->list output-list)
                               (make-immutable-hash (hash->list assign-hash))))
            (provide ld-system))))]))

(provide (rename-out [dsl-expand #%module-begin]) #%datum #%app #%top)

(require rackunit rackunit/text-ui syntax/macro-testing)
(define constant-tests
  (test-suite
   "Constant expansion"
   [check-equal? (phase1-eval #'(addition (constant 2) (constant 3)))
                 '(constant (+ 2 3))
                 "Simple addition collapse"]
   [check-equal? (phase1-eval #'(addition (constant 2) (negation (constant 3))))
                 '(constant (+ 2 (- 3)))
                 "Simple addition of negative collapse"]
   [check-equal? (phase1-eval #'(addition (constant 2) (addition (constant 3) (addition (constant 4) (constant 5)))))
                 '(constant (+ 2 (+ 3 (+ 4 5))))
                 "Simple addition of negative collapse"]
   [check-equal? (phase1-eval #'(subtraction (constant 2) (constant 3)))
                 '(constant (- 2 3))
                 "Simple subtraction collapse"]
   [check-equal? (phase1-eval #'(multiplication (constant 3) (constant 4)))
                 '(constant (* 3 4))
                 "Simple multiplication collapse"]
   [check-equal? (phase1-eval #'(multiplication (constant 3) (addition (constant 4) (constant 5))))
                 '(constant (* 3 (+ 4 5)))
                 "Nested addition in multiplication right term"]
   [check-equal? (phase1-eval #'(multiplication (addition (constant 4) (constant 5)) (constant 3)))
                 '(constant (* (+ 4 5) 3))
                 "Nested addition in multiplication left term"]
   [check-equal? (phase1-eval #'(multiplication (addition (constant 4) (constant 5)) (addition (constant 3) (constant 2))))
                 '(constant (* (+ 4 5) (+ 3 2)))
                 "Nested addition in multiplication both term"]
   [check-equal? (phase1-eval #'(division (constant 3) (constant 4)))
                 '(constant (/ 3 4))
                 "Simple division collapse"]
   [check-equal? (phase1-eval #'(division (constant 3) (addition (constant 4) (constant 5))))
                 '(constant (/ 3 (+ 4 5)))
                 "Nested addition in division right term"]
   [check-equal? (phase1-eval #'(division (addition (constant 4) (constant 5)) (constant 3)))
                 '(constant (/ (+ 4 5) 3))
                 "Nested addition in division left term"]
   [check-equal? (phase1-eval #'(division (addition (constant 4) (constant 5)) (addition (constant 3) (constant 2))))
                 '(constant (/ (+ 4 5) (+ 3 2)))
                 "Nested addition in division both term"]))

(test-suite
 "Power negative sign handling"
 [check-equal? (phase1-eval #'(vss "-" 9.0))
               '(vss -9.0) "VSS substitute"]
 [check-equal? (phase1-eval #'(vdd "-" 9.0))
               '(vdd -9.0) "VDD substitute"]
 [check-equal? (phase1-eval #'(gnd "-" 9.0))
               '(gnd -9.0) "GND substitute"])

(require rackunit/text-ui)
;; (run-tests constant-tests)
