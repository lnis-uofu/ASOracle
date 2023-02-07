#lang racket
(require linear_diff/expander linear_diff/synthesis linear_diff/resistors)
(require racket/format)
(provide write-verilog)

(define (eia-name eia) "foo")
(define (format-resistor resistor)
  (format "~d" (* (resistor-value resistor) (resistor-order resistor))))
(define (format-capacitor capacitor)
  (format "~d" (* (capacitor-value capacitor) (capacitor-order capacitor))))

(define (write-integrator port id assign)
  (match assign
    [(integrator capacitor resistor output inputs)
     (write port "integrator_~a integrator_~a\n" (length inputs) id)
     (write port "#(.CAPACITOR(~a),\n," (format-capacitor capacitor))
     (for ([(resistor _) inputs] [i (in-naturals)])
       (write port ".RESISTOR_~a(~a),\n" i (format-resistor resistor)))
     (write port ")\n(")
     (write port ".OUTPUT(~a),\n" output)
     (for ([(_ net) inputs] [i (in-naturals)])
       (write port ".INPUT_~a(~a),\n" i net))
     (write port ");")]))

(define (write-inverting port id assign)
  (match assign
    [(inverting feedback output inputs)
     (write port "inverting_~a inverting_~a\n" (length inputs) id)
     (write port "#(.FEEDBACK(~a)\n," (format-resistor feedback))
     (for ([(resistor _) inputs] [i (in-naturals)])
       (write port ".RESISTOR_~a(~a),\n" i (format-resistor resistor)))
     (write port ")\n(")
     (write port ".OUTPUT(~a),\n" output)
     (for ([(_ net) inputs] [i (in-naturals)])
       (write port ".INPUT_~a(~a),\n" i net))
     (write port ");")]))

(define (write-noninverting port id assign)
  (match assign
    [(non-inverting feedback output inputs)
     (write port "noninverting_~a noninverting_~a\n" (length inputs) id)
     (write port "#(.FEEDBACK(~a)\n," (format-resistor feedback))
     (for ([(resistor _) inputs] [i (in-naturals)])
       (write port ".RESISTOR_~a(~a),\n" i (format-resistor resistor)))
     (write port ")\n(")
     (write port ".OUTPUT(~a),\n" output)
     (for ([(_ net) inputs] [i (in-naturals)])
       (write port ".INPUT_~a(~a),\n" i net))
     (write port ");")]))

(define (write-divider port id assign)
  (match assign
    [(divider output top-net top bottom-net bottom)
     (write port "divider divider_~a\n" id)
     (write port "#(")
     (write port ".TOP(~a)\n," (format-resistor top))
     (write port ".BOTTOM(~a)\n," (format-resistor bottom))
     (write port ")\n(")
     (write port ".OUTPUT(~a),\n" output)
     (write port ".TOP_IN(~a)\n," top-net)
     (write port ".BOTTOM_IN(~a)" bottom-net)
     (write port ");")]))

;; Would be nice to be polymorphic instead
(define (write-module port id assign)
  (let ([serializer (match assign
                        [(integrator _ _ _ _) write-integrator]
                         [(inverting _ _ _) write-inverting]
                         [(non-inverting _ _ _) write-noninverting]
                         [(divider _ _ _ _ _) write-divider])])
    (serializer port id assign)))

(define (output-net assign) 'foo)

(define (write-wires port inputs outputs assigns)
  (for ([assign assigns])
    (let ([net (output-net assign)])
      (if (and (not (member assign inputs)) (not (member assign outputs)))
          (write port "wire ~a;" assign) 'void))))

(define (write-verilog mapped port)
  (match mapped
      [(ld-program vdd vss gnd design eia inputs outputs assigns)
       (write port "module ~a (\n" design)
       (write port "input vdd, vss, gnd,")
       (for ([input inputs])
         (write port "input ~a,\n" input))
       (for ([output outputs])
         (write port "output ~a,\n" output))
       (write port ");\n\n")
       (write port "parameter VDD = ~a;\n" vdd)
       (write port "parameter VSS = ~a;\n" vss)
       (write port "parameter GND = ~a;\n" gnd)
       (write port "parameter EIA = ~a;\n\n" (eia-name eia))
       (write-wires port inputs outputs assigns)
       (for ([assign assigns] [i (in-naturals)])
         (write-module port assign i))
       (write port "endmodule;")]))
