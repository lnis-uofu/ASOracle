#lang racket

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