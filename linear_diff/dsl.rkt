#lang racket

;;;;;;;; DSL AST post expansion ;;;;;;;;
(provide (struct-out ld-program)
         (struct-out negative)
         (struct-out add)
         (struct-out multiply)
         (struct-out integrate)
         (struct-out const)
         (struct-out reference))

(struct ld-program (vdd vss gnd design eia inputs outputs assigns) #:mutable #:transparent)
(struct integrate (expr) #:transparent)
(struct add (left right) #:transparent)
(struct negative (expr) #:transparent)
(struct multiply (constant expr) #:transparent)
(struct const (scalar) #:transparent)
(struct reference (name) #:transparent)

;;;;;;;; DSL post optimization ;;;;;;;
(provide (struct-out summing)
         (struct-out integrating)
         (struct-out weight))
(struct summing (inl inr) #:transparent)
(struct integrating (in) #:transparent)
(struct weight (value id) #:transparent)

;;;;;;;; DSL techmapped ;;;;;;;;
(provide (struct-out integrator)
         (struct-out inverting)
         (struct-out non-inverting)
         (struct-out divider)
         (struct-out resistor)
         (struct-out capacitor))

(struct integrator (capacitor resistor output inputs) #:transparent)
(struct inverting (feedback output inputs) #:transparent)
(struct non-inverting (feedback output inputs) #:transparent)
(struct divider (output top-net top bottom-net bottom) #:transparent)
(struct resistor (value order) #:transparent)
(struct capacitor (value order) #:transparent)