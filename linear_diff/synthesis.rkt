#lang racket
(require "expander.rkt" "resistors.rkt")

(provide synthesizer
         synthesize-expr
         (struct-out integrator)
         (struct-out inverting)
         (struct-out non-inverting)
         (struct-out divider)
         (struct-out resistor)
         (struct-out capacitor))

;; (add x (mult y z)) => (add (mult 1 x) (mult y z)) all summations children are multiplication, multiply 1 if not a multiply.

;; Optimizations:
;; all summations or integration should be wrapped in either negation or double negation.
;; push negations up from children to summation.
;; if more children are negation, negate the summation and distribute negation. otherwise double negate.

;;;; Devices
;; Vout(t1) = Vout(t0) - 1/RC * (integral 0 to t1 Vin(t)dt)
(struct integrator (capacitor resistor output-net input-nets) #:transparent)
;; Inputs are pairs '(resistor . input-net). Summation is inverting.
(struct inverting (feedback output-net input-nets) #:transparent)
;; Non-inverting amplifier, gain is 1 + rf/r1
(struct non-inverting (feedback output-net input-net) #:transparent)
;; Preference would probably be inverting amplifier rather than a
;; passive divider, but is an option, especially for reference voltages.
(struct divider (output-net resistor-top input-top resistor-bottom input-bottom) #:transparent)
;;(struct net-port (symbol output-net))
(struct resistor (value order precision) #:transparent)
(struct capacitor (value order precision) #:transparent)

(define (output-net e) 'x)

(define (expand-sum-child synth feedback)
  (lambda (expr)
    (match expr
      [(negative (multiply scalar child))
       (list (feedback-closest feedback scalar) (synth (negative child)))]
      [(multiply scalar child)
       (list (feedback-closest feedback scalar) (synth child))]
      [child
       (list feedback (synth child))])))
(struct sum (child-))



(define (normalize-sum-child normalize)
  (lambda (expr)
    (match expr
      [(negative (multiply scalar child))
       (multiply scalar (negative (normalize child)))]
      [(multiply scalar child)
       (multiply scalar (normalize child))]
      [child
       (multiply (constant 1) (normalize child))])))

(define (collapse-add expr)
  (match expr
    [(add a b)
     (let ([x (collapse-add a)]
           [y (collapse-add b)])
       (match (list x y)
                [(list (sum j) (sum k )) (sum (append j k))]
                [(list (sum j) k) (sum (cons k j))]
                [(list j (sum k)) (sum (cons j k))]
                [(list j k) (sum '(j k))]))]
    [(integrate y) (integrate (collapse-add y))]
    [(negative y) (negative (collapse-add y))]
    [(multiply x y) (multiply (collapse-add x) (collapse-add y))]
    [x x]))

(define (normalize expr)
  (match expr
    ;; integral also can sum
    [(negative (integrate (sum child-exprs)))
     (negative (integrate (sum (map (normalize-sum-child normalize ) child-exprs))))]

    [(integrate (negative (sum child-exprs)))
     (negative (integrate (sum (map (curry normalize-sum-child normalize) child-exprs))))]

    [(integrate (sum child-exprs))
     (negative (negative (integrate
                          (sum (map (curry normalize-sum-child normalize) child-exprs)))))]

    [(integrate child-expr)
(negative (negative (integrate (sum (curry normalize-sum-child normalize) child-expr))))]

    [(negative (sum child-exprs))
     (negative (sum (map (curry normalize-sum-child normalize) child-exprs)))]

    [(sum child-exprs)
     (negative (negative (sum (map (curry normalize-sum-child normalize) child-exprs))))]

    [(negative (multiply scalar child-expr))
     (negative (multiply scalar (normalize child-expr)))]

    [(negative child-expr)
     (negative (normalize child-expr))]

    [(multiply scalar child-expr)
     (multiply scalar (normalize child-expr))]

    [x x]))

(define (child-for-feedback eia feedback child)
  (match child [(multiply (constant scalar) input) (list (feedback-closest eia feedback scalar) input)]))

(define (synthesize-expr eia power-vdd power-vss expr)
  (letrec ([synth (lambda (expr)
    (match expr
      ;; integral also can sum
      [(negative (integrate (sum child-exprs)))
       (integrator (gensym)
                   (resistor 1 10 5)
                   (capacitor 1 -10 20)
                   (map
                    (compose
                     (match-lambda [(list res child) (list res (synth child))])
                     (curry child-for-feedback eia (resistor 1 10 5)))
                    child-exprs))]

      [(negative (sum child-exprs))
       (let* (
              [feedback (resistor 1 'k 1)]
              [child-transform (compose (match-lambda [(list res child) (list res (synth child))])
                                        (curry child-for-feedback eia feedback))]
              [children (map child-transform child-exprs)]
              [id (gensym)])
       (inverting id feedback children))]

      [(negative child-expr)
       (inverting (gensym) (resistor 1 3 1) (synth child-expr))]

      [(negative (multiply scalar child-expr))
       (inverting (gensym) (feedback-closest scalar) (synth child-expr))]

      [(multiply scalar child-expr)
       (negative (synth (negative expr)))]
      [(constant scalar)
       (match-let ([(cons order (cons top bottom)) (divider-closest eia power-vdd power-vss scalar)])
         (divider (gensym)
                  (resistor top order 10)
                  power-vdd
                  (resistor bottom order 10)
                  power-vss))]

      [ref ref]))])
    (synth (normalize (collapse-add expr)))))

(define (synthesizer program) (curry synthesize-expr
                     (ld-program-eia program)
                     (ld-program-vdd program)
                     (ld-program-vss program)))
;; (define (synthesize program)
;;   (let ([synthesized
;;          (map
;;               (ld-program-assigns program))])
;;     (struct-copy ld-program program [assigns synthesized])))

(define (flatten-network expr)
  'void
  ;; (match expr
  ;;   [(integrator output capacitor output inputs)
  ;;    (match inputs
  ;;      [(list (list resistor input)...)
  ;;        ;;
  ;;        (let* ([children (for/list ([r resistor] [i input]) (list r (output-net input)))]
  ;;               [neighbors (map flatten-network input)]
  ;;               [flattened (integrator output capacitor resistor children)])
  ;;          (cons flattened neighbors))])]

  ;;   [(inverting feedback output inputs)
  ;;    (match inputs
  ;;      [(list (list resistor input)...)
  ;;        (let* ([children (for/list ([r resistor] [i input]) (list r (output-net input)))]
  ;;               [neighbors (map flatten-network input)]
  ;;               [flattened (inverting output capacitor resistor children)])
  ;;          (cons flattened neighbors))])]

  ;;   [(non-inverting feedback output inputs)
  ;;    (match inputs
  ;;      [(list (list resistor input)...)
  ;;        (let* ([children (for/list ([r resistor] [i input]) (list r (output-net input)))]
  ;;               [neighbors (map flatten-network input)]
  ;;               [flattened (non-inverting output capacitor resistor children)])
  ;;          (cons flattened neighbors))])]

  ;;   [(divider output top top-net bottom bottom-net)
  ;;    (match-let ([(list tr ti) top]
  ;;                [(list br bi) bottom])
  ;;      (let* (
  ;;             [top (tr (output-net ti))]
  ;;             [bottom (br (output-net bi))]
  ;;             [neighbors (append (flatten-network ti) (flatten-network bi))]
  ;;             [flattened (divider output-net top bottom)])
  ;;        (cons flattened neighbors)))]

  ;;   [_ '()])
  );; (reference c) '()]))

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
