#lang racket
(require "expander.rkt" "resistors.rkt")
(provide synthesize)

;; (add x (mult y z)) => (add (mult 1 x) (mult y z)) all summations children are multiplication, multiply 1 if not a multiply.

;; Optimizations:
;; all summations or integration should be wrapped in either negation or double negation.
;; push negations up from children to summation.
;; if more children are negation, negate the summation and distribute negation. otherwise double negate.

;;;; Devices
;; Vout(t1) = Vout(t0) - 1/RC * (integral 0 to t1 Vin(t)dt)
(struct integrator (capacitor resistor output-net input-nets))
;; Inputs are pairs '(resistor . input-net). Summation is inverting.
(struct inverting (feedback output-net input-nets))
;; Non-inverting amplifier, gain is 1 + rf/r1
(struct non-inverting (feedback output-net input-net))
;; Preference would probably be inverting amplifier rather than a
;; passive divider, but is an option, especially for reference voltages.
(struct divider (output-net input-top input-bottom))
;;(struct net-port (symbol output-net))
(struct resistor (value order precision))
(struct capacitor (value order precision))

(define (expand-sum-child synth feedback)
  (lambda (expr)
    (match expr
      [(negative (multiply scalar child))
       (list (feedback-closest feedback scalar) (synth (negative child)))]
      [(multiply scalar child)
       (list (feedback-closest feedback scalar) (synth child))]
      [child
       (list feedback (synth child))])))
(struct sum (child-exprs))

(define (collapse-add expr)
  (match expr
    [(add a b)
     (let ([x (collapse-add a)]
           [y (collapse-add b)])
       (match (list x y)
                [(list (sum j) (sum k )) (sum (append j k))]
                [(list (sum j) k) (sum (cons k j))]
                [(list j (sum k)) (sum (cons j k))]
                [(list j k) (sum j k)]))]
    [(integrate y) (integrate (collapse-add y))]
    [(negative y) (negative (collapse-add y))]
    [(multiply x y) (multiply (collapse-add x) (collapse-add y))]
    [x x]))

(define (normalize expr)
  (match expr
    ;; integral also can sum
    [(negative (integrate (sum child-exprs)))
     (negative (integrate (sum (map (compose normalize expand-sum-child) child-exprs))))]

    [(integrate (negative (sum child-exprs)))
     (negative (integrate (sum (map (compose normalize expand-sum-child) child-exprs))))]

    [(integrate (sum child-exprs))
     (negative (negative (integrate
                          (sum (map (compose normalize expand-sum-child) child-exprs)))))]

    [(integrate child-expr)
(negative (negative (integrate (sum (compose normalize expand-sum-child) child-expr))))]

    [(negative (sum child-exprs))
     (negative (sum (map (compose normalize expand-sum-child) child-exprs)))]

    [(sum child-exprs)
     (negative (negative (sum (map (compose normalize expand-sum-child) child-exprs))))]

    [(negative (multiply scalar child-expr))
     (negative (multiply scalar (normalize child-expr)))]

    [(negative child-expr)
     (negative (normalize child-expr))]

    [(multiply scalar child-expr)
     (multiply scalar (normalize child-expr))]

    [x x]))

(define (child-for-feedback feedback child)
  (match child [(multiply scalar input) (list (feedback-closest feedback scalar) input)]))

(define (synthesize eia power-vdd power-vss expr)
  (letrec ([synth (lambda (expr)
    (match expr
      ;; integral also can sum
      [(negative (integrate (sum child-exprs)))
       (integrator (gensym)
                   (resistor 1 10 5)
                   (capacitor 1 -10 20)
                   (map
                    (compose (curry child-for-feedback (resistor 1 10 5))
                             (lambda (res child) (list res (synth child))))
                    child-exprs))]

      [(negative (sum child-exprs))
       (let* (
              [feedback (resistor 1 'k 1)]
              [child-transform (compose (curry child-for-feedback feedback)
                                        (lambda (res child) (list res (synth child))))]
              [children (map child-transform child-exprs)]
              [id (gensym)])
       (inverting feedback id children))]

      [(negative child-expr)
       (inverting (gensym) (resistor 1 3 1) (resistor 1 3 1) (synth child-expr))]

      [(negative (multiply scalar child-expr))
       (inverting (gensym) (feedback-closest scalar) (synth child-expr))]

      [(multiply scalar child-expr)
       (synth (negative (negative expr)))]
      [(constant scalar) (divider (gensym) (divider-closest eia power-vdd power-vss scalar))]

      [ref ref]))])
  (synth (collapse-add expr))))

(define (flatten-network expr)
  (match expr
    [(integrator output capacitor output inputs)
     (match inputs
       ([(list (list resistor input)...)
         ;;
         (let* ([children (for/list ([r resistor] [i input]) (list r (output-net input)))]
                [neighbors (map flatten-network input)]
                [flattened (integrator output capacitor resistor children)])
           (list flattened neighbors))]))]

    [(inverting feedback output inputs)
     (match inputs
       ([(list (list resistor input)...)
         (let* ([children (for/list ([r resistor] [i input]) (list r (output-net input)))]
                [neighbors (map flatten-network input)]
                [flattened (inverting output capacitor resistor children)])
           (list flattened neighbors))]))]

    [(noninverting feedback output inputs)
     (match inputs ([((resistor input)...)
                     (let* (
                           [children (for/list ([r resistor] [i input]) (list r (output-net input)))]
                           [neighbors (map flatten-network input)]
                           [flattened (noninverting output capacitor resistor children)])
                       (cons flattened neighbors))]))]

    [(divider output top bottom)
     (match inputs ([((resistor t) ti) top]
                    [((resistor b) bi) bottom]
                     (let* (
                            [top (t (output-net ti))]
                            [bottom (b (output-net bi))]
                            [neighbors (append  (flatten-network ti) (flatten-network bi))]
                            [flattened (integrator output-net capacitor resistor children)])
                       (cons flattened neighbors))))]

    [(reference c) '()]))

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
;; [(net-port (symbol))]
;; [(input-port (output-net))]
;; [(output-port (input-net))]
;; [(resistor (value order precision))]
;; [(capacitor (value order precision))]
;; )
