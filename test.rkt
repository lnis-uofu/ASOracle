#lang racket
(require racket/file
         linear_diff/parser
         linear_diff/lexer
         linear_diff/expander
         linear_diff/synthesis)
(require "example.rkt")
(define parsed (parse (tokenize "example.rkt" (open-input-file "example.rkt"))))

(define (synthesize-expr program) (lambda (expr)
  (synthesize (ld-program-eia program)
              (ld-program-vdd program)
              (ld-program-vss program)
              (expr))))

(define (s expr)
  (synthesize (ld-program-eia ld-system)
              (ld-program-vdd ld-system)
              (ld-program-vss ld-system)
              (hash-ref (ld-program-assigns ld-system) expr)))
;; (define p (dsl-read-syntax "example.rkt"))
;; (map (synthesize-expr ld-system) (hash-values (ld-program-assigns ld-system)))
