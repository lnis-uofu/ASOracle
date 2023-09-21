#lang racket
(require "dsl.rkt" "ldegg.rkt")

(provide synthesize expand-program)

(define (synthesize program)
  (match-let* ([assigns (ld-program-assigns program)]
               [outputs (ld-program-outputs program)]
               [exprs (expand-outputs outputs assigns)]
               [rules '()]
               [query (make-egg-query (map (match-lambda [(list _ b) b]) exprs) rules)]
               [(cons variants proof) (run-egg query #f)])
    variants))

(define (expand-program program)
  (expand-outputs (ld-program-outputs program) (ld-program-assigns program)))

(define (expand-outputs outputs assigns)
  (map (lambda (name) (list name (expand (reference name) assigns))) outputs))

(define (expand expr assigns)
  (match expr
    [(integrate child) (integrate (expand child assigns))]
    [(add left right) (add (expand left assigns) (expand right assigns))]
    [(negative child) (negative (expand child assigns))]
    [(multiply left right) (multiply (expand left assigns) (expand right assigns))]
    [(const scalar) (const scalar)]
    [(reference name) (if (hash-has-key? assigns name)
                          (expand (hash-ref assigns name) assigns)
                          (reference name))]))

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
  (define expand-tests
    (test-suite
     "Expand outputs"
     [test-case "Expand input"
       (check-equal? (expand (reference 'a) (hash 'c (reference 'b)))
                     (reference 'a))]
     [test-case "Expand reference to input"
       (check-equal? (expand (reference 'a) (hash 'a (reference 'b)))
                     (reference 'b))]
     [test-case "Expand reference chain"
       (check-equal? (expand (reference 'a) (hash 'a (reference 'b) 'b (reference 'c)))
                     (reference 'c))]
     [test-case "Expand constant"
       (check-equal? (expand (const 4) (hash))
                     (const 4))]
     [test-case "Expand nest integrate"
       (check-equal? (expand (integrate (reference 'b)) (hash 'b (add (reference 'c) (reference 'd))))
                     (integrate (add (reference 'c) (reference 'd))))]
     [test-case "Expand nested add"
       (check-equal? (expand (add (integrate (reference 'b)) (multiply (reference 'c) (const 4))) (hash 'b (add (reference 'c) (reference 'd))))
                     (add (integrate (add (reference 'c) (reference 'd))) (multiply (reference 'c) (const 4))))]
     [test-case "Expand nest integrate"
       (check-equal? (expand (negative (reference 'b)) (hash 'b (integrate (reference 'c))))
                     (negative (integrate (reference 'c))))]))
  (run-tests flatten-tests)
  (run-tests expand-tests))
