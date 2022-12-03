#lang racket
(require (prefix-in brag- brag/support))
(require racket/file linear_diff/expander (prefix-in dsl- linear_diff/reader) linear_diff/parser linear_diff/lexer)

(define parsed (parse (tokenize "example.rkt" (open-input-file "example.rkt"))))
;; (define p (dsl-read-syntax "example.rkt"))
