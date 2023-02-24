#lang racket
(require "resistors.rkt" "dsl.rkt")

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
;; Non-inverting amplifier, gain is 1 + rf/r1, BUT it shifts the phase so not really usable.

;; Preference would probably be inverting amplifier rather than a
;; passive divider, but is an option, especially for reference voltages.
(struct divider (output-net resistor-top resistor-bottom) #:transparent)
;;(struct net-port (symbol output-net))
(struct resistor (value order precision) #:transparent)
(struct capacitor (value order precision) #:transparent)

(struct operation-child (scalar reference))
(struct negated-integration (children))
(struct negated-multiplication (scalar children))

(define (expand-sum-child synth feedback)
  (lambda (expr)
    (match expr
      [(negative (multiply scalar child))
       (list (feedback-closest feedback scalar) (synth (negative child)))]
      [(multiply scalar child)
       (list (feedback-closest feedback scalar) (synth child))]
      [child
       (list feedback (synth child))])))
(struct sum (children))



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

(define (make-graph-exprs gensym exprs)
  (let ([fold (lambda (expr results)
                (match-let ([(list symbols flattened references) results]
                            [(list child-symbol child-flattened child-references)
                             (make-graph-expr gensym expr)])
                  (list (cons child-symbol symbols)
                        (append child-flattened flattened)
                        (append child-references references))))])
  (foldl fold '('() '() '()) exprs)))

;; get the child symbol(s), the inner flattened, and the internal references
;; create a copy of this element with the child symbol
;; merge this into the flattened set.
(define (make-graph-expr gensym expr)
  (let ([output-symbol (gensym)])
    (match expr
      [(negative child)
       (match-let* ([(list symbol nested references) (make-graph-expr gensym child)]
                    [copy (negative symbol)])
         (list output-symbol (output-symbol (negative symbol)) references))]
      [(integrate children)
          (match-let* ([(list child-symbols nested references) (make-graph-exprs gensym children)]
                       [copy (integrate child-symbols)])
            (list output-symbol (cons (list output-symbol copy) nested) references))]
      [(sum children)
          (match-let* ([(list child-symbols nested references) (make-graph-exprs gensym children)]
                       [copy (sum child-symbols)])
            (list output-symbol (cons (list output-symbol copy) nested) references))]
      [(multiply left right)
          (match-let* ([(list left-symbol left-nested left-references) (make-graph-expr gensym left)]
                       [(list right-symbol right-nested right-references) (make-graph-expr gensym right)]
                       [copy (multiply left-symbol right-symbol)])
            (list output-symbol
                  (cons (list output-symbol copy) (append left-nested right-nested))
                  (append left-references right-references)))]
      [(constant _)
       (list output-symbol (list (list output-symbol expr)) '())]
      [(variable x)
       (list x '() (list x))])))

(define (make-graph gensym assigns)
  (match-let* ([fold (lambda (assign results)
                (match-let* ([(list assign-symbol expr) assign]
                             [(list flattened references) results]
                             [(list _ assign-flattened assign-references)
                              (make-graph-expr gensym expr)])
                  (list (append assign-flattened flattened)
                        (append assign-references references))))]
               [(list flattened references) (foldl fold '('() '()) (hash->list assigns))]
               [graph (make-hash flattened)]
               [found (set-subtract (list->set assigns) (list->set (hash-keys flattened)))])
    (list graph found)))

(module+ synthesis-test
  (require rackunit rackunit/text-ui syntax/macro-testing)

  (define (gensym-counter initial)
    (let ([counter initial])
      (lambda ([prefix 'g])
        (begin (string->symbol (format "~a~a" prefix counter))
               (set! counter (add1 counter))))))

  (define synth (curry synthesize-expr eia-12 -9 9))
  (define collapse-addition-tests
    (test-suite
     [test-case "Two term addition"
       (check-equal? (collapse-add (add (variable 'a) (variable 'b)))
                     (sum (list (variable 'a) (variable 'b))))]
     [test-case "Nested right addition"
       (check-equal? (collapse-add (add (variable 'a) (add (variable 'b) (variable 'c))))
                     (sum (list (variable 'a) (variable 'b) (variable 'c))))]
     [test-case "Nested left addition"
       (check-equal? (collapse-add (add (add (variable 'a) (variable 'b)) (variable 'c)))
                     (sum (list (variable 'a) (variable 'b) (variable 'c))))]
     [test-case "Nested both addition"
       (check-equal? (collapse-add (add (add (variable 'a) (variable 'b)) (add (variable 'c) (variable 'd))))
                     (sum (list (variable 'a) (variable 'b) (variable 'c) (variable 'd))))]
     [test-case "Internal addition to an integrate"
       (check-equal? (collapse-add (integrate (add (variable 'a) (variable 'b))))
                     (integrate (sum (list (variable 'a) (variable 'b)))))]
     [test-case "Internal addition to a multiply"
       (check-equal? (collapse-add (multiply (constant 1) (add (variable 'a) (variable 'b))))
                     (multiply (constant 1) (sum (list (variable 'a) (variable 'b)))))]
     [test-case "Internal addition to a negative"
       (check-equal? (collapse-add (negative (add (variable 'a) (variable 'b))))
                     (negative (sum (list (variable 'a) (variable 'b)))))]))
  (define flatten-graph-tests
    (test-suite
     [test-case "Recognize a transitive constant reference"
       (match-let* ([assigns (make-hash 'a (variable 'b)
                                  'b (constant 1))]
                    [(list graph found) (make-graph (gensym-counter 0) assigns)])
         (check-equal? assigns graph)
         (check-equal? found '()))]
     [test-case "Recognize found constant"
       (match-let* ([assigns (make-hash 'a (variable 'b))]
                    [(list graph found) (make-graph (gensym-counter 0) assigns)])
         (check-equal? found (list 'b)))]
     [test-case "Flatten an addition"
       (match-let* ([assigns (make-hash 'a (sum (constant 1) (negative 'b))
                                  'b (constant 1))]
                    [(list graph found) (make-graph (gensym-counter 0) assigns)])
         (check-equal? (hash-ref graph 'a (sum 'g0 'g1)))
         (check-equal? (hash-ref graph 'b (constant 1)))
         (check-equal? (hash-ref graph 'g0 (constant 1)))
         (check-equal? (hash-ref graph 'g1 (negative 'b)))
         (check-equal? found (list 'b 'g0 'g1)))]))

  (define synthesis-tests
    (test-suite
     [test-case "Synthesize a divider"
       (let ([result (synth (constant 4))])
         (check-true (divider? result)))]
     ;; [test-case "Synthesize a nested addition"
     ;;   (let ([result (synth (sum (constant


     ;;    (check-equal? (
    ))
  (run-tests collapse-addition-tests)
  (run-tests flatten-graph-tests)
  (run-tests synthesis-tests))
