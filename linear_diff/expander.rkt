#lang racket
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/stx))
(require "resistors.rkt")

(provide ld-program vss gnd vdd negation integration multiplication addition subtraction eia input output design assign eia-96 eia-48 eia-24 eia-12)
(provide dsl-expand negative add subtract multiply integrate constant variable)

;;;;;;;; DSL AST post expansion ;;;;;;;;
(struct ld-program (vdd vss gnd design eia inputs outputs assigns) #:mutable)
(struct integrate (expr))
(struct add (left right))
(struct subtract (left right))
(struct negative (expr))
(struct multiply (left right))
(struct divide (left right))
(struct constant (scalar))
(struct reference (name))

(define (setup-system lines)
  (define system (ld-program))
  (for ([line lines]) (apply line system)))

;;;;;;;; Line expansion ;;;;;;;;
(define-for-syntax (line-identifier ctx) (lambda (line)
  (syntax-case line (eia input output design vdd vss gnd assign)
    [(eia series) (format-id #'ctx "eia-series" #:source #'line)]
    [(input id) (format-id #'ctx "input-~a" #'id #:source #'line)]
    [(output id) (format-id #'ctx "output-~a" #'id #:source #'line)]
    [(design _) (format-id #'ctx "design-name" #:source #'line)]
    [(vdd _ ...) (format-id #'ctx "power-vdd" #:source #'line)]
    [(vss _ ...) (format-id #'ctx "power-vss" #:source #'line)]
    [(gnd _ ...) (format-id #'ctx "power-gnd" #:source #'line)]
    [(assign id _) (format-id #'ctx "assign-~a" #'id #:source #'line)])))

(define-syntax (eia stx)
  (with-syntax
    ([ident ((line-identifier stx) stx)])
    (syntax-case stx ()
      [(eia series)
       #'(define (ident system)
           (set-ld-program-eia! system series))])))

(define-syntax (design stx)
  (with-syntax
    ([ident ((line-identifier stx) stx)])
    (syntax-case stx ()
      [(design name)
        #'(define (ident system)
            (set-ld-program-design! system 'name))])))

(define-syntax (vdd stx)
  (with-syntax
    ([ident ((line-identifier stx) stx)])
    (syntax-case stx ()
      [(vdd _ n)
       #'(vdd (- n))]
      [(vdd n)
       #'(define (ident system)
           (set-ld-program-vdd! system n))])))

(define-syntax (vss stx)
  (with-syntax
    ([ident ((line-identifier stx) stx)])
    (syntax-case stx ()
      [(vss _ n)
       #'(vss (- n))]
      [(vss n)
       #'(define (ident system)
           (set-ld-program-vss! system n))])))

(define-syntax (gnd stx)
  (with-syntax
    ([ident ((line-identifier stx) stx)])
    (syntax-case stx ()
      [(gnd _ n)
       #'(gnd (- n))]
      [(gnd n)
       #'(define (ident system)
           (set-ld-program-gnd! system n))])))

(define-syntax (assign stx)
  (with-syntax
    ([ident ((line-identifier stx) stx)])
    (syntax-case stx ()
      [(_ name expr)
       #'(define (ident system)
           (hash-set! (ld-program-assigns system) 'name expr))])))

(define-syntax (input stx)
  (with-syntax
    ([ident ((line-identifier stx) stx)])
    (syntax-case stx ()
      [(_ name)
       #'(define (ident system)
           (set-ld-program-inputs! system
            (cons 'name (ld-program-inputs system))))])))

(define-syntax (output stx)
  (with-syntax
    ([ident ((line-identifier stx) stx)])
    (syntax-case stx ()
      [(_ name)
       #'(define (ident system)
           (set-ld-program-outputs! system
            (cons 'name (ld-program-outputs system))))])))

;;;;;;;; Expressions ;;;;;;;;
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

(define-syntax-rule (variable name)
  (reference 'name))

;;;;;;;; Language expansion ;;;;;;;;
(define-syntax (dsl-expand stx)
  (syntax-case stx ()
    [(dsl-expand (linear (line content) ...))
     (with-syntax ([(id ...) (stx-map (line-identifier #'(content ...)) #'(content ...))])
       #'(#%module-begin
          content ...
          (define ld-system (setup-system id ...))
          (provide ld-system)))]))

(provide (rename-out [dsl-expand #%module-begin]) #%datum #%app #%top)

;;;;;;;; Tests ;;;;;;;;

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
