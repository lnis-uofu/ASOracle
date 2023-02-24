#lang racket

(provide (struct-out ld-program)
         (struct-out negative)
         (struct-out add)
         (struct-out multiply)
         (struct-out integrate)
         (struct-out constant)
         (struct-out variable))

;;;;;;;; DSL AST post expansion ;;;;;;;;
(struct ld-program (vdd vss gnd design eia inputs outputs assigns) #:mutable #:transparent)
(struct integrate (expr) #:transparent)
;; TODO should move the summation collagpse from synthesis to here
(struct add (left right) #:transparent)
(struct negative (expr) #:transparent)
(struct multiply (constant expr) #:transparent)
(struct constant (scalar) #:transparent)
(struct variable (name) #:transparent)
