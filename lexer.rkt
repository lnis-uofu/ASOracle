#lang racket
(require (prefix-in brag- brag/support))
(require br-parser-tools/lex)
(require (prefix-in : br-parser-tools/lex-sre))

;;(require parser-tools/lex)
;;(require (prefix-in : parser-tools/lex-sre))

(provide tokenize)

(define (tokenize ip)
  (port-count-lines! ip)
  (define my-lexer
    (lexer-src-pos
     ["\n"
      (brag-token 'NEWLINE lexeme #:skip? #t)]
     [(:seq (:+ numeric) (:? (:: "." (:+ numeric))))
      (brag-token 'NUMBER (string->number lexeme))]
     [(:: "/*" (complement (:: any-string "*/" any-string)) "*/")
      (brag-token 'COMMENT lexeme #:skip? #t)]
     [(:seq "#" (:* (char-complement "\n")))
      (brag-token 'COMMENT lexeme #:skip? #t)]
     [whitespace
      (brag-token 'WHITESPACE lexeme #:skip? #t)]
     [(:: "eia-" (:or "96" "48" "24" "12"))
      (brag-token 'EIA lexeme)]
     [","
      (brag-token lexeme lexeme)]
     [(char-set "*+-()=")
      (brag-token lexeme lexeme)]
     [(:or "vdd" "vss" "gnd" "design" "input" "output" "integrate" )
      (brag-token lexeme lexeme)]
     [(:: (:+ alphabetic) (:? (:: "_" (:+ alphabetic))))
      (brag-token 'IDENTIFIER lexeme)]
     [(eof) 'eof]))
  (define (next-token)
    (let ([n (my-lexer ip)])
      (if (eq? 'eof (position-token-token n)) eof n)))
  next-token)
