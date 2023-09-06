#lang racket
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))

(require "resistors.rkt" "dsl.rkt")

(provide vss gnd vdd negation integration multiplication constant
         addition subtraction eia input output design assign variable
         eia-96 eia-48 eia-24 eia-12)

(provide dsl-expand)

(define (setup-system lines)
  (define system (ld-program 0 0 0 "test" eia-96 empty empty (make-hash)))
  (for ([line lines]) (line system))
  system)

;;;;;;;; Line expansion ;;;;;;;;
(define-for-syntax (line-identifier line)
  (syntax-case line (eia input output design vdd vss gnd assign)
    [(eia series) (format-id #'line "eia-series" #:source #'line)]
    [(input id) (format-id #'line "input-~a" #'id #:source #'line)]
    [(output id) (format-id #'line "output-~a" #'id #:source #'line)]
    [(design _) (format-id #'line "design-name" #:source #'line)]
    [(vdd _ ...) (format-id #'line "power-vdd" #:source #'line)]
    [(vss _ ...) (format-id #'line "power-vss" #:source #'line)]
    [(gnd _ ...) (format-id #'line "power-gnd" #:source #'line)]
    [(assign id _) (format-id #'line "assign-~a" #'id #:source #'line)]))

(define-syntax (eia stx)
  (syntax-case stx ()
    [(_ series)
     #'(lambda (system)
         (set-ld-program-eia! system series))]))

(define-syntax (design stx)
  (syntax-case stx ()
    [(_ name)
     #'(lambda (system)
         (set-ld-program-design! system 'name))]))

(define-syntax (vdd stx)
  (syntax-case stx ()
    [(_ _ n)
     #'(vdd (- n))]
    [(_ n)
     #'(lambda (system)
         (set-ld-program-vdd! system n))]))

(define-syntax (vss stx)
  (syntax-case stx ()
    [(_ _ n)
     #'(vss (- n))]
    [(_ n)
     #'(lambda (system)
         (set-ld-program-vss! system n))]))

(define-syntax (gnd stx)
  (syntax-case stx ()
    [(_ _ n)
     #'(gnd (- n))]
    [(_ n)
     #'(lambda (system)
         (set-ld-program-gnd! system n))]))

(define-syntax (assign stx)
  (syntax-case stx ()
    [(_ name expr)
     #'(lambda (system)
         (hash-set! (ld-program-assigns system) 'name expr))]))

(define-syntax (input stx)
  (syntax-case stx ()
    [(_ name)
     #'(lambda (system)
         (set-ld-program-inputs! system
           (cons 'name (ld-program-inputs system))))]))

(define-syntax (output stx)
  (syntax-case stx ()
    [(_ name)
     #'(lambda (system)
         (set-ld-program-outputs! system
          (cons 'name (ld-program-outputs system))))]))

(define-for-syntax (expand-expr stx)
  (syntax-case stx (constant negation integration division multiplication subtraction addition variable)
    [(constant c) #'(constant c)]
    [(variable v) #'(variable 'v)]
    [(negation expr)
     (syntax-case (expand-expr #'expr) (constant)
       [(constant c) #'(constant (- c))]
       [n #'(negative n)])]
    [(integration expr)
     (syntax-case (expand-expr #'expr) (negative)
       [(negative x) #'(negative (integrate x))]
       [n #'(integrate n)])]
    [(division left right)
     (syntax-case (list (expand-expr #'left) (expand-expr #'right)) (constant)
       [((constant l) (constant r)) #'(constant (/ l r))])]
    [(multiplication left right)
     ;; left side multiplication must be a constant term.
     (syntax-case (list (expand-expr #'left) (expand-expr #'right)) (constant)
       [((constant t) (constant u)) #'(constant (* t u))]
       [(_ (constant 0)) #'(constant 0)]
       [((constant 0) _) #'(constant 0)]
       [(y (constant x))
        #'(multiply (constant x) y)]
       [((constant x) y)
          #'(multiply (constant x) y)])]
    [(subtraction left right)
     (expand-expr #'(addition left (negation right)))]
    [(addition left right)
     (syntax-case (list (expand-expr #'left) (expand-expr #'right)) (constant)
       [((constant t) (constant u)) #'(constant (+ t u))]
       [x #'(add x)])]))

;;;;;;;; Expressions ;;;;;;;;
(define-syntax (negation stx) (expand-expr stx))
(define-syntax (integration stx) (expand-expr stx))
(define-syntax (division stx) (expand-expr stx))
(define-syntax (multiplication stx) (expand-expr stx))
(define-syntax (subtraction stx) (expand-expr stx))
(define-syntax (addition stx) (expand-expr stx))

;;;;;;;; Language expansion ;;;;;;;;
(define-syntax (dsl-expand stx)
  (syntax-case stx ()
    [(dsl-expand (linear (line content) ...))
     #'(#%module-begin
          (define parsed-system (setup-system (list content ...)))
          (provide parsed-system))]))

(provide (rename-out [dsl-expand #%module-begin]) #%datum #%app #%top #%top-interaction)

;;;;;;;; Tests ;;;;;;;;
(module+ test
  (require rackunit rackunit/text-ui syntax/macro-testing)
  (define constant-tests
    (test-suite
     "Constant expansion"
     [test-case "Simple addition collapse"
       (check-equal? (addition (constant 2) (constant 3))
                     (constant 5))]
     [test-case "Simple addition of negative collapse"
       (check-equal? (addition (constant 2) (negation (constant 3)))
                     (constant -1))]
     [test-case "Simple addition of negative collapse"
       (check-equal? (addition (constant 2) (addition (constant 3) (addition (constant 4) (constant 5))))
                     (constant (+ 2 (+ 3 (+ 4 5)))))]
     [test-case "Simple subtraction collapse"
       (check-equal? (subtraction (constant 2) (constant 3))
                     (constant (- 2 3)))]
     [test-case "Negative subtraction collapse"
       (check-equal? (subtraction (constant 2) (negation (constant 3)))
                     (constant (- 2 (- 3))))]
     [test-case "Simple multiplication collapse"
       (check-equal? (multiplication (constant 3) (constant 4))
                     (constant (* 3 4)))]
     [test-case "Nested addition in multiplication right term"
       (check-equal? (multiplication (constant 3) (addition (constant 4) (constant 5)))
                     (constant (* 3 (+ 4 5))))]
     [test-case "Nested addition in multiplication left term"
       (check-equal? (multiplication (addition (constant 4) (constant 5)) (constant 3))
                     (constant (* (+ 4 5) 3)))]
     [test-case "Nested addition in multiplication both term"
       (check-equal? (multiplication (addition (constant 4) (constant 5)) (addition (constant 3) (constant 2)))
                     (constant (* (+ 4 5) (+ 3 2))))]
     [test-case "Simple division collapse"
       (check-equal? (division (constant 3) (constant 4))
                     (constant (/ 3 4)))]
     [test-case "Nested addition in division right term"
       (check-equal? (division (constant 3) (addition (constant 4) (constant 5)))
                     (constant (/ 3 (+ 4 5))))]
     [test-case "Nested addition in division left term"
       (check-equal? (division (addition (constant 4) (constant 5)) (constant 3))
                     (constant (/ (+ 4 5) 3)))]
     [test-case "Nested addition in division both term"
       (check-equal? (division (addition (constant 4) (constant 5))
                               (addition (constant 3) (constant 2)))
                     (constant (/ (+ 4 5) (+ 3 2))))]
     [test-case "Non-constant division"
       (check-exn exn:fail:syntax?
                  (lambda () (convert-syntax-error
                              (division (constant 4) (variable 'a)))))]
     [test-case "Non-constant multiplication"
       (check-exn exn:fail:syntax?
                  (lambda () (convert-syntax-error
                              (multiplication (variable 'b) (variable 'a)))))]))


  (define line-tests (test-suite
   "Power negative sign handling"
   [test-case "VSS substitution"
     (check-equal? (ld-program-vss (setup-system (list (vss "-" 9.0))))
                   -9.0)]
   [test-case "VDD substitute"
     (check-equal? (ld-program-vdd (setup-system (list (vdd "-" 9.0))))
                   -9.0)]
   [test-case "GND substitute"
     (check-equal? (ld-program-gnd (setup-system (list (gnd "-" 9.0))))
                   -9.0)]))

  (run-tests constant-tests)
  (run-tests line-tests))
