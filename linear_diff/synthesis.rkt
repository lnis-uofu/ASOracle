#lang racket
(require "resistors.rkt" "dsl.rkt")

(define (synthesize program) program)

(define (flatten assigns)
  (append (map (match-lambda [(list symbol expr) (flatten-expr symbol expr gensym)]) assigns)))
  
(define (flatten-expr symbol expr gensym)
  (match expr
    [(integrate child)
     (match-let ([(list id exprs) (flatten-expr (gensym) child gensym)])
       (list symbol (cons (list symbol (integrate id)) exprs)))]
    [(add left right)
     (match-let ([(list left_id left_exprs) (flatten-expr (gensym) left gensym)]
                 [(list right_id right_exprs) (flatten-expr (gensym) right gensym)])
       (list symbol (cons (list symbol (add left_id right_id)) (append left_exprs right_exprs))))]
    [(negative child)
     (match-let ([(list id exprs) (flatten-expr (gensym) child gensym)])
       (list symbol (cons (list symbol (negative id)) exprs)))]
    [(multiply left right)
     (match-let ([(list left_id left_exprs) (flatten-expr (gensym) left gensym)]
                 [(list right_id right_exprs) (flatten-expr (gensym) right gensym)])
       (list symbol (cons (list symbol (multiply left_id right_id)) (append left_exprs right_exprs))))]
    [(const scalar)
     (list symbol (list (list symbol (const scalar))))]
    [(reference name)
     (list name '())]))

(struct summing (inl inr))
(struct integrating (in))
(struct weight (value id))

(module+ test
  (require rackunit rackunit/text-ui )
  (define (gensym-counter initial)
    (let ([counter initial])
      (lambda ([prefix 'g])
        (let ([symb (string->symbol (format "~a~a" prefix counter))])
          (begin (set! counter (add1 counter))
               symb)))))
  
  (define flatten-tests
    (test-suite
     "Flatten expressions"
     [test-case "Flatten integrate"
      (check-equal? (flatten-expr 'a (integrate (const 4)) (gensym-counter 0))
                    (list 'a
                          (list (list 'a (integrate 'g0))
                                (list 'g0 (const 4)))))]
     [test-case "Flatten addition with two children"
      (check-equal? (flatten-expr 'a (add (const 4) (reference 'b)) (gensym-counter 0))
                    (list 'a
                          (list (list 'a (add 'g0 'b))
                                (list 'g0 (const 4)))))]
     [test-case "Flatten nested"
      (check-equal? (flatten-expr 'a (negative (multiply (const 5) (reference 'b))) (gensym-counter 0))
                    (list 'a
                          (list (list 'a (negative 'g0))
                                (list 'g0 (multiply 'g1 'b))
                                (list 'g1 (const 5)))))]))

  (run-tests flatten-tests))