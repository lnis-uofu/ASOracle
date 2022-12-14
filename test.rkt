#lang racket
(require racket/file linear_diff/parser linear_diff/lexer)
(require "example.rkt")
(define parsed (parse (tokenize "example.rkt" (open-input-file "example.rkt"))))
;; (define p (dsl-read-syntax "example.rkt"))
