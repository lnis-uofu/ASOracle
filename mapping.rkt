#lang racket
;; (define-syntax (addition stx)
;;   (syntax-case stx (summation constant)
;;     [(addition (constant t) (constant u))
;;      #'(constant (+ t u))]
;;     [(addition (summation a) (summation b))
;;      #'(summation a b)]
;;     [(addition (summation a ...) b)
;;      #'(summation a ... b)]
;;     [(addition a (summation b ...))
;;      #'(summation a b ...)]
;;     [(addition a b)
;;      #'(summation a b)]))

;; (add x (mult y z)) => (add (mult 1 x) (mult y z)) all summations children are multiplication, multiply 1 if not a multiply.
;; (with-syntax ([(summantion ...) j]
     ;;               [(addition ...) k])
     ;;   (addition!
     ;;   )]
    ;; [(addition (multiplication j k) (multiplication l m))
    ;;  #'(addition (multiplication j k) (multiplication l m))]
    ;; [(addition j (multiplication k l))
    ;;  #'(addition (multiplication 1.0 j) (multiplication k l))]
    ;; [(addition! (multiplication j k) l)
    ;;  #'(addition (multiplication j k) (multiplication 1.0 l))]
    ;; [(addition j k)
    ;;  (with-syntax ([(addition ...) #'j]
    ;;                [(addition ...) #'k])
    ;;    #'(addition )
    ;;  #'(addition! (multiplication 1.0 j) (multiplication 1.0 k))]))



;; Optimizations:
;; all summations or integration should be wrapped in either negation or double negation.
;; push negations up from children to summation.
;; if more children are negation, negate the summation and distribute negation. otherwise double negate.

;;
;; (define rewrite-sum-child (rewrite feedback term)
;;   (match expr
;;     [(negative (multiply scalar child))
;;      (multiply scalar (rewrite (negative child)))]
;;     [(multiply scalar child)
;;      (multiply scalar (rewrite child))]
;;     [child
;;      (multiply 1 rewrite)]
;;     child-exprs))

;; (define (rewrite expr)
;;   (match expr
;;     ;; integral also can sum
;;     ;; [(integration initial start end (sum expr) delta) #f]
;;     [(negative (integration initial start end delta (sum child-exprs)))
;;      (negative (integration intitial start end delta (sum (map (curry rewrite-sum-child rewrite) child-exprs))))]
;;     [(integration initial start end delta (negative(sum child-exprs)))
;;      (negative (integration intitial start end delta (sum (map (curry rewrite-sum-child rewrite) child-exprs))))]
;;     [(integration initial start end delta child-expr)
;;      (negative (integration initial start end delta (rewrite (negative child-expr)))]
;;     [(sum (list item empty)) (rewrite item)]
;;     [(negative (sum child-exprs))
;;      (negative (sum (map (curry rewrite-sum-child rewrite) child-exprs)))
;;     [(sum exprs)
;;      (negative (sum (map (curry rewrite-sum-child rewrite) (map negative child-exprs))))]
;;     [(negative (negative child-expr))
;;      (rewrite child-expr)]
;;     [_ expr]
;; ]))

;; (define expand-inverting-feedback (synth feedback terms)
;;   (map (lambda (expr)
;;          (match expr
;;            [(negative (multiply scalar child))
;;             (list (feedback-closest feedback scalar) (synth (negative child)))]
;;            [(multiply scalar child)
;;             (list (feedback-closest feedback scalar) (synth child))]
;;            [child
;;             (list feedback (synth child))]))
;;          child-exprs))

(define (synthesize eia expr)
  (letrec
      ([synth (lambda (expr)
                (match expr
                 ;; integral also can sum
                 ;; [(integrate initial start end (sum expr) delta) #f]
                  [(negative (integrate initial start end delta (sum child-exprs)))
                   (integrator (resistor 1 10 5)
                               (capacitor 1 -10 20)
                               delta
                               (expand-inverting-feedback synth (resistor 1 10 5) child-exprs))]
                  [(integrate initial start end delta (negative(sum child-exprs)))
                   (integrator (resistor 1 10 5)
                               (capacitor 1 -10 20)
                               delta
                               (expand-inverting-feedback synth (resistor 1 10 5) child-exprs))]
                  [(integrate initial start end delta child-expr)
                   (synth (negative (negative expr)))]
                  [(negative (sum child-exprs))
                   (inverting (resistor 1 'k 1)
                              (expand-inverting-feedback synth (resistor 1 3 1) child-exprs))]
                  [(sum exprs)
                   (synth (negative (negative expr)))]
                  [(negative child-expr)
                   (inverting (resistor 1 3 1) (resistor 1 3 1) (synth child-expr))]
                  [(assign net child-expr) (net-port net (synth child-expr))]
                  [(negative (multiply scalar child-expr))
                   (inverting (feedback-closest scalar) (synth child-expr))]
                  [(multiply scalar child-expr)
                   (synth (negative (negative expr)))]
                  [(input net) (input-port net)]
                  [(output net) (output-port net)]
                  [symbol? (net-port (synth expr))]
                  [(constant scalar) (divider (divider-closest eia vdd vss scalar))]
                  ))])
    (synth expr)))

;;;; Devices
;; Vout(t1) = Vout(t0) - 1/RC * (integral 0 to t1 Vin(t)dt)
(struct integrator (capacitor resistor output-net input-nets))
;; Inputs are pairs '(resistor . input-net). Summation is inverting.
(struct inverting (feedback output-net input-nets))
;; Non-inverting amplifier, gain is 1 + rf/r1
(struct non-inverting (feedback resistor output-net input-net))
;; Preference would probably be inverting amplifier rather than a
;; passive divider, but is an option, especially for reference voltages.
(struct divider (top_resistor bottom_resistor output-net top-input-net bottom-input-net))
;;(struct net-port (symbol output-net))
(struct input-port (output-net))
(struct output-port (input-net))
(struct resistor (value order precision))
(struct capacitor (value order precision))
(struct vdd (voltage))
(struct vss (voltage))

(define expand-inverting-feedback (synth feedback terms)
  (map (lambda (expr)
         (match expr
           [(negative (multiply scalar child))
            (list (feedback-closest feedback scalar) (synth (negative child)))]
           [(multiply scalar child)
            (list (feedback-closest feedback scalar) (synth child))]
           [child
            (list feedback (synth child))]))
         child-exprs))

(define (synthesize eia expr)
  (letrec
      ([synth (lambda (expr)
                (match expr
                 ;; integral also can sum
                 ;; [(integrate initial start end (sum expr) delta) #f]
                  [(negative (integrate initial start end delta (sum child-exprs)))))
                (integrator (resistor 1 10 5)
                            (capacitor 1 -10 20)
                            delta
                            (expand-inverting-feedback synth (resistor 1 10 5) child-exprs))]
              [(integrate initial start end delta (negative(sum child-exprs))) delta)
                (integrator (resistor 1 10 5)
                          (capacitor 1 -10 20)
                          delta
                          (expand-inverting-feedback synth (resistor 1 10 5) child-exprs))
                [(integrate initial start end delta child-expr)
                 (synth (negative (negative expr))]]
                [(negative (sum child-exprs))
                 (inverting (resistor 1 'k 1)
                            (expand-inverting-feedback synth (resistor 1 3 1) child-exprs))]
                [(sum exprs)
                 (synth (negative (negative expr))))]
                 [(negative child-expr)
                  (inverting (resistor 1 3 1) (resistor 1 3 1) (synth child-expr))]
                 [(assign net child-expr) (net-port net (synth child-expr))]
                 [(negative (multiply scalar child-expr))
                  (inverting (feedback-closest scalar) (synth child-expr))]
                 [(multiply scalar child-expr)
                  (synth (negative (negative expr)))
                 [(input net) (input-port net)]
                 [(output net) (output-port net)]
                 [symbol? (net-port (synth expr))]
                 [(constant scalar) (divider (divider-closest eia vdd vss scalar))]
                 ))])
  (synth expr))
)

;; Flatten all nested structures to nets.
;; Define top level module with inputs and outptus.
;; Define all wires
;; Generate all modules.
;; (define flatten-network (network)
;;   (match
;;       [(integrator (capacitor resistor input-net))
;;        (let [child (flatten-network input-net)]
;;          [child-net (first (first child))]
;;          [net (net-port (gensym "integrator_"))]
;;          [instance (integrator capacitor resistor inner-net)])
;;        (cons (list net instance) child)]
;;     [(inverting (feedback inputs))
;;       (let [child (flatten-network input-net)]
;;          [child-net (first (first child))]
;;          [net (net-port (gensym "integrator_"))]
;;          [instance (integrator capacitor resistor inner-net)])
;;        (cons (list net instance) child)]
;; ;; Non-inverting amplifier, gain is 1 + rf/r1
;; [(non-inverting (feedback resistor input-net))
;; ;; Preference would probably be inverting amplifier rather than a
;; ;; passive divider, but is an option, especially for reference voltages.
;; [(divider (top top_res bottom bottom_res))
;; [(net-port (symbol))
;; [(input-port (output-net))
;; [(output-port (input-net))
;; [(resistor (value order precision))
;; [(capacitor (value order precision))
;; )
