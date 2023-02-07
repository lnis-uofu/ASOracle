#lang racket
(require "synthesis.rkt")
(provide write-spice)
(define (write-spice n) 'void)
;; (define (eia-name eia) "foo")

;; TODO format to standard suffix notation
;; (define (format-resistor resistor)
;;   (~d (* (resistor-value resistor) (resistor-scale resistor))))

;; (define (write-integrator port assign id)
;;   (match ([(integrator capacitor output inputs)
;;            (write port "Xintegrator_~a " id)
;;            (write port " ~a" output)
;;            (for ([(_ net) inputs] [i (in-naturals)]) (write port " ~a" net))
;;            (write port "integrator_~a" (size inputs))
;;            (write port " CAPACITOR=~a," (format-capacitor capacitor))
;;            (for ([(resistor _) inputs] [i (in-naturals)]) (write port " RESISTOR_~a=~a" i (format-resistor resistor)))])))

;; (define (write-integrator port assign id)
;;   (match ([(inverting feedback output inputs)
;;            (write port "Xinverting_~a " id)
;;            (write port " ~a" output)
;;            (for ([(_ net) inputs] [i (in-naturals)]) (write port " ~a" net))
;;            (write port "inverting_~a" (size inputs))
;;            (write port " FEEDBACK=~a" (format-resistor feedback))
;;            (for ([(resistor _) inputs] [i (in-naturals)]) (write port " RESISTOR_~a=~a" i (format-resistor resistor)))])))

;; (define (write-integrator port assign id)
;;   (match ([(inverting feedback output inputs)
;;            (write port "Xnoninverting_~a " id)
;;            (write port " ~a" output)
;;            (for ([(_ net) inputs] [i (in-naturals)]) (write port " ~a" net))
;;            (write port "noninverting_~a" (size inputs))
;;            (write port " FEEDBACK=~a" (format-resistor feedback))
;;            (for ([(resistor _) inputs] [i (in-naturals)]) (write port " RESISTOR_~a=~a" i (format-resistor resistor)))])))

;; (define (write-divider port assign id)
;;   (match ([(divider output top bottom)
;;            (write port "Xdivider_~a" id)
;;            (write port " ~a" output)
;;            (write port (match ([(_ source) top])
;;                          (format " ~a" source)))
;;            (write port (match ([(_ source) bottom])
;;                          (format " ~a" source)))
;;            (write port " divider")
;;            (write port (match ([(resistor _) top])
;;                          (format port " TOP=~a" (format-resistor feedback))))
;;            (write port (match ([(resistor _) bottom])
;;                          (format " BOTTOM=~a" (format-resistor feedback))))])))

;; ;; Would be nice to be polymorphic instead
;; (define (write-module port assign id)
;;   (let ([serializer (match
;;                         ([(integrator _ ...) write-integrator]
;;                          [(inverting _ ...) write-inverting]
;;                          [(noninverting _ ...) write-noninverting]
;;                          [(divider _ ...) write-divider]))])
;;     (serializer port assign id)))

;; (define (write-spice mapped port)
;;   (match
;;       ([(ld-program vdd vss name eia inputs outputs assigns)
;;         (let ([out (open-output-file filename)])
;;           (write port "Simulation ~a\n" name)
;;           (write port ".param vdd ~a\n" vdd)
;;           (write port ".param vss ~a\n" vss)
;;           (write port ".param gnd ~a\n" gnd)
;;           (for ([assign assigns] [i (in-naturals)]) (write-module port assign i)))])))
