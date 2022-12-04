#lang racket
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))

(provide ld-program vss gnd vdd negation integration multiplication addition subtraction eia input output design assign)
(provide dsl-expand negative add subtract multiply integrate constant variable)

;;;; DSL AST post expansion
;; operator expressions
(struct ld-program (vdd vss gnd design eia inputs outputs assigns) #:mutable)
(struct integrate (expr))
(struct add (left right))
(struct subtract (left right))
(struct negative (expr))
(struct multiply (left right))
(struct divide (left right))
(struct constant (scalar))
(struct id (name))

(define (eia-lookup name) (list 1 2 3))
(define (system lines) 'todo)

(define-syntax-rule (eia name)
  (define (eia-series system)
    (system-set-eia (eia-lookup name))))


(define-syntax (design stx)
  (syntax-case stx ()
    [(design name)
      (with-syntax
        ([ident (format-id #'name "design-~a" #'name)])
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

(define-syntax (assign stx)
  (syntax-case stx ()
    [(_ name expr)
     (with-syntax
       ([ident (format-id #'name "assign-~a" #'name)])
       #'(define ident expr))]))

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

(define-syntax-rule (variable name) (id 'name))

(define-syntax (input stx)
  (syntax-case stx ()
    [(_ name)
      (with-syntax
        ([ident (format-id #'name "input-~a" #'name)])
        #'(define ident 'name))]))

(define-syntax (output stx)
  (syntax-case stx ()
    [(_ name)
      (with-syntax
        ([ident (format-id #'name "output-~a" #'name)])
        #'(define ident 'name))]))

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
    [(dsl-expand (linear (line lines) ...))
     (with-syntax ([((id _ ...) ...) #'(lines ...)])
;;                   [((id ...) ...) #'contents])
       #'(#%module-begin
          lines ...
          (define ld-system (id ...))
          (provide ld-system)))]))

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
