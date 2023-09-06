#lang racket

(provide (struct-out ld-program)
         (struct-out negative)
         (struct-out add)
         (struct-out multiply)
         (struct-out integrate)
         (struct-out const)
         (struct-out var))

;;;;;;;; DSL AST post expansion ;;;;;;;;
(struct ld-program (vdd vss gnd design eia inputs outputs assigns) #:mutable #:transparent)
(struct integrate (expr) #:transparent)
(struct add (left right) #:transparent)
(struct negative (expr) #:transparent)
(struct multiply (constant expr) #:transparent)
(struct const (scalar) #:transparent)
(struct var (name) #:transparent)
