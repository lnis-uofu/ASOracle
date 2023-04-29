#lang racket
(require "resistors.rkt" "dsl.rkt")
(require data/queue)

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
(struct operation-child (scalar reference))
(struct negated-integration (children))
(struct negated-multiplication (scalar children))

(define (normalize-sum-child expr assigns)
  (match (hash-ref expr assigns)
    [(negative child)
     (match (hash-ref child assigns)
       [
     (list (operation-child 1 child) ]
    [(multiply scalar child)
     (list (operation-child scalar child) '() '())]
    [child
     (operation-child 1 child)]))

(define (normalize-sum-children children assigns) 'void)

(define (normalize-sum-child-neg child assigns) 'void)

(define (normalize-sum-children-neg children assigns) 'void)


;; return:
;; the normalized expression for given.
;; new graph nodes
;; nodes to add to queue
(define (normalize id assigns)
  (match (hash-ref id assigns)
    [(negative child)
     (match (hash-ref child assigns)
       [(negative subchild)
        (normalize subchild assigns)]
       [(integrate child)
        (match-let
            ([(list child-ids new-assigns)  (normalize-sum-child child assigns)])
          (list (negated-integration child-ids) new-assigns child-ids))]
       [(multiply (constant scalar) child) #:when (>= scalar 0)
        (match-let
            ([(list child-ids new-assigns) (normalize-sum-child child assigns)])
          (list (negated-multiplication scalar child-ids) new-assigns child-ids))]
       [(multiply (constant scalar) child) #:when (< scalar 0)
        (match-let
            ([(list child-ids new-assigns) (normalize-sum-child-neg child assigns)])
          (list (negated-multiplication (- scalar) child-ids) new-assigns child-ids))]
       [(sum children)
        (match-let
            ([(list child-ids new-assigns) (normalize-sum-children children assigns)])
          (list (negated-multiplication 1 child-ids) new-assigns child-ids))]
       [(constant scalar) 'void] ;; todo constant
       )]
    [(multiply (constant scalar) child) #:when (>= scalar 0)
     (match-let
         ([(list child-ids new-assigns) (normalize-sum-children-neg children assigns)])
       (list (negated-multiplication scalar child-ids) new-assigns child-ids))]
    [(multiply (constant scalar) child) #:when (< scalar 0)
     (match-let
         ([(list child-ids new-assigns) (normalize-sum-children children assigns)])
       (list (negated-multiplication (- scalar) child-ids) new-assigns child-ids))]
    [(integrate child)
     (match-let
         ([(list child-ids new-assigns) (normalize-sum-children-neg children assigns)])
       (list (negated-integration scalar child-ids) new-assigns child-ids))]
    [(sum children)
     (match-let
         ([(list child-ids new-assigns) (normalize-sum-children-neg children assigns)])
       (list (negated-multiplication 1 child-ids) new-assigns child-ids))]
    [(constant scalar) (list (constant scalar) '() '())]))

  ;; (match expr
  ;;   ;; integral also can sum
  ;;   [(negative (integrate (sum child-exprs)))
  ;;    (negative (integrate
  ;;               (sum (map (normalize-sum-child normalize ) child-exprs))))]
  ;;   [(integrate (negative (sum child-exprs)))
  ;;    (negative (integrate
  ;;               (sum (map (curry normalize-sum-child normalize) child-exprs))))]
  ;;   [(integrate (sum child-exprs))
  ;;    (negative (integrate
  ;;               (sum (map (lambda (child-expr) (normalize-sum-child normalize (negative child-expr))) child-exprs))))]
  ;;   [(integrate child-expr)
  ;;    (negative (integrate
  ;;               (sum (normalize-sum-child normalize (negative child-expr)))))]
  ;;   [(negative (sum child-exprs))
  ;;    (negative (sum (map (curry normalize-sum-child normalize) child-exprs)))]
  ;;   [(sum child-exprs)
  ;;    (negative (sum
  ;;               (map (lambda (child-expr) (normalize-sum-child normalize (negative child-expr))) child-exprs)))]
  ;;   [(negative (multiply scalar child-expr))
  ;;    (negative (multiply scalar (normalize child-expr)))]
  ;;   [(multiply scalar child-expr)
  ;;    (negative (multiply scalar (normalize (negative child-expr))))]
  ;;   [(negative child-expr)
  ;;    (negative (normalize child-expr))]))


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
