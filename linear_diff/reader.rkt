#lang racket

(require linear_diff/parser linear_diff/lexer linear_diff/dsl syntax/strip-context)

(provide (rename-out [dsl-read read]
                     [dsl-read-syntax read-syntax]))

(define (dsl-read in)
  (syntax->datum
   (dsl-read-syntax #f in)))

(define (dsl-read-syntax path port)
  (define parse-tree (parse path (tokenize path port)))
  (strip-context
   #`(module linear-mod linear_diff/expander
       #,parse-tree)))
