#lang racket
(require "parser.rkt" "lexer.rkt" syntax/strip-context)

(provide (rename-out [ld-read read]
                     [ld-read-syntax read-syntax]))
(provide ld-read-syntax ld-read)

(define (ld-read in)
  (syntax->datum
   (ld-read-syntax #f in)))

(define (ld-read-syntax path port)
  (define parse-tree (parse path (tokenize port path)))
  (strip-context
   #`(module linear_diff_thing "expander.rkt"
       #,parse-tree)))
