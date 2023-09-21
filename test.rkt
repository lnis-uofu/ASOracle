#lang racket
(require racket/file
         linear_diff/parser
         linear_diff/lexer
         linear_diff/expander
         linear_diff/synthesis
         linear_diff/ldegg
         graph)

(provide parsed synthesized exprs eggs)

(require "example.rkt")
(define parsed (parse (tokenize "example.rkt" (open-input-file "example.rkt"))))
(define synthesized (synthesize parsed-system))
(define synthesized-top (synthesize-combined parsed-system))
(define exprs (expand-program parsed-system))
(define eggs (map (match-lambda [(list a expr) (expr->egg-expr expr '())]) exprs))
(println "***** separate *****")
(for-each pretty-display synthesized)
(println "***** top *****")
(for-each pretty-display synthesized-top)
(println "***** graph *****")

(println (graphviz (flatten-results synthesized-top)
                   #:output (open-output-file	"test.dot")))
;                   #:vertex-attributes (map (match-lambda [(cons name)
