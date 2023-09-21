#lang racket
(require egg-ld)
(require ffi/unsafe)
(require "dsl.rkt")
(module+ test (require rackunit))
(provide make-egraph
         egraph-add-expr egraph-run-rules
         egraph-get-simplest egraph-get-variants
         egraph-get-proof egraph-is-unsound-detected
         remove-rewrites run-egg make-egg-query
         expr->egg-expr
         (struct-out egraph-query))
;;;;;;;; Config
;; The maximum size of an egraph
(define *node-limit* (make-parameter 8000))
(define *proof-max-length* (make-parameter 200))
(define *proof-max-string-length* (make-parameter 10000))

;;;;;;;; Rules
(struct rule (name input output) #:transparent)
;;;;;;;;; Serialization
(define (expr->egg-expr expr _)
  (match expr
    [(integrate child) (format "(e& ~a)" (expr->egg-expr child _))]
    [(add left right) (format "(e+ ~a ~a)" (expr->egg-expr left _) (expr->egg-expr right _))]
    [(negative child) (format "(e- ~a)" (expr->egg-expr child _))]
    [(multiply left right) (format "(e* ~a ~a)" (expr->egg-expr left _) (expr->egg-expr right _))]
    [(const scalar) (~a scalar)]
    [(reference name) (~a name)]))

(define (egg-s-expr->expr expr)
  (match expr
    [(list "&" child) (integrating (egg-s-expr->expr child))]
    [(list "$" left right) (summing (egg-s-expr->expr left)
                                    (egg-s-expr->expr right))]
    [(list "*" const child) (weight const (egg-s-expr->expr child))]
    [else expr]))

(define (egg-expr->expr str egraph-data)
 (egg-s-expr->expr (read (open-input-string str))))

(define (egg-exprs->exprs str egraph-data)
  (egg-s-expr->expr (read (open-input-string str))))

;;;;;;;; Runtime

;; the first hash table maps all symbols and non-integer values to new names for egg
;; the second hash is the reverse of the first
(struct egraph-data (egraph-pointer egg->local-dict local->egg-dict))

;; Racket bound version of an egg runner
;; Defines parameters for running rewrite rules with egg
(struct egraph-query (exprs rules iter-limit node-limit const-folding?) #:transparent)

(define (make-egg-query exprs rules
                        #:iter-limit [iter-limit #f]
                        #:node-limit [node-limit (*node-limit*)]
                        #:const-folding? [const-folding? #t])
  (egraph-query exprs rules iter-limit node-limit const-folding?))

(define (run-egg input variants?
                 #:proof-input [proof-input '()]
                 #:proof-ignore-when-unsound? [proof-ignore-when-unsound? #f])
  (define egg-graph (make-egraph))
  (define node-ids (map (curry egraph-add-expr egg-graph) (egraph-query-exprs input)))
  (define iter-data (egraph-run-rules egg-graph
                                      (egraph-query-node-limit input)
                                      (egraph-query-rules input)
                                      node-ids
                                      (egraph-query-const-folding? input)
                                      #:limit (egraph-query-iter-limit input)))


  (println "***** iter-data *****")
  ;(print iter-data)
  (define variants
    (if variants?
        (for/list ([id node-ids] [expr (egraph-query-exprs input)])
          (egraph-get-variants egg-graph id expr))
        (for/list ([id node-ids])
          (for/list ([iter (in-range (length iter-data))])
            (egraph-get-simplest egg-graph id iter)))))
  ;(println "***** Variants *****")
  ;(print variants)
  (match proof-input
    [(cons start end)
     #:when (not (and (egraph-is-unsound-detected egg-graph) proof-ignore-when-unsound?))
     (when (not (egraph-is-equal egg-graph start end))
       (error "Cannot get proof: start and end are not equal.\n start: ~a \n end: ~a" start end))

     (define proof (egraph-get-proof egg-graph start end))
       (print "***** Variants *****")
     ;(write proof)
     (when (null? proof)
       (error (format "Failed to produce proof for ~a to ~a" start end)))
     (cons variants proof)]
    [else (cons variants #f)]))

(define (egraph-get-simplest egraph-data node-id iteration)
  (define ptr (egraph_get_simplest (egraph-data-egraph-pointer egraph-data) node-id iteration))
  (define str (cast ptr _pointer _string/utf-8))
  (destroy_string ptr)
  (egg-expr->expr str egraph-data))

(define (egraph-get-variants egraph-data node-id orig-expr)
  (define expr-str (~a (expr->egg-expr orig-expr egraph-data)))
  (define ptr (egraph_get_variants (egraph-data-egraph-pointer egraph-data) node-id expr-str))
  (define str (cast ptr _pointer _string/utf-8))
  (destroy_string ptr)
  (egg-exprs->exprs str egraph-data))

(define (egraph-is-unsound-detected egraph-data)
  (egraph_is_unsound_detected (egraph-data-egraph-pointer egraph-data)))

(define (egraph-get-cost egraph-data node-id iteration)
  (egraph_get_cost (egraph-data-egraph-pointer egraph-data) node-id iteration))

(define (egraph-get-times-applied egraph-data rule)
  (egraph_get_times_applied (egraph-data-egraph-pointer egraph-data) (FFIRule-name rule)))

(define (egraph-stop-reason egraph-data)
  (match (egraph_get_stop_reason (egraph-data-egraph-pointer egraph-data))
    [0 "saturated"]
    [1 "iter limit"]
    [2 "node limit"]
    [3 "unsound"]
    [sr (error 'egraph-stop-reason "unexpected stop reason ~a" sr)]))

(define (make-raw-string s)
  (define b (string->bytes/utf-8 s))
  (define n (bytes-length b))
  (define ptr (malloc 'raw (+ n 1)))
  (memcpy ptr b n)
  (ptr-set! ptr _byte n 0)
  ptr)

(define (make-ffi-rule rule)
  (define name (make-raw-string (~a (rule-name rule))))
  (define lhs (make-raw-string (~a (rule-input rule))))
  (define rhs (make-raw-string (~a (rule-output rule))))
  (make-FFIRule name lhs rhs))

(define (free-ffi-rule rule)
  (free (FFIRule-name rule))
  (free (FFIRule-left rule))
  (free (FFIRule-right rule))
  (free rule))

; Makes a new egraph that is managed by Racket's GC
(define (make-egraph)
  (egraph-data (egraph_create) (make-hash) (make-hash)))

(define (remove-rewrites proof)
  (match proof
    [`(Rewrite=> ,rule ,something)
     (remove-rewrites something)]
    [`(Rewrite<= ,rule ,something)
     (remove-rewrites something)]
    [(list _ ...)
     (map remove-rewrites proof)]
    [else proof]))

;; Performs a product, but traverses the elements in order
;; This is the core logic of flattening a proof given flattened proofs for each child of a node
(define (sequential-product elements)
  (cond
    [(empty? elements) (list empty)]
    [else
     (define without-rewrites (remove-rewrites (last (first elements))))
     (append
      (for/list ([head (first elements)])
        (cons head (map first (rest elements))))
      (for/list ([other (in-list (rest (sequential-product (rest elements))))])
        (cons without-rewrites other)))]))

(module+ test
  (check-equal?
   (sequential-product `((1 2) (3 4 5) (6)))
   `((1 3 6) (2 3 6) (2 4 6) (2 5 6)))

  (expand-proof-term '(Explanation (+ x y) (+ y x)) (box 10)))


;; returns a flattened list of terms
;; The first term has no rewrite- the rest have exactly one rewrite
(define (expand-proof-term term budget)
  (match term
    [(? (lambda (x) (<= (unbox budget) 0)))
     (list #f)]
    [`(Explanation ,body ...)
     (expand-proof body budget)]
    [(? symbol?)
     (list term)]
    [(? number?)
     (list term)]
    [(? list?)
     (define children (map (curryr expand-proof-term budget) term))
     (cond
       [(member (list #f) children)
        (list #f)]
       [else
        (define res (sequential-product children))
        (set-box! budget (- (unbox budget) (length res)))
        res])]
    [else (error "Unknown proof term ~a" term)]))


;; Remove the front term if it doesn't have any rewrites
(define (remove-front-term proof)
  (if (equal? (remove-rewrites (first proof)) (first proof))
      (rest proof)
      proof))

;; converts a let-bound tree explanation
;; into a flattened proof
(define (expand-proof proof budget)
  (define expanded
    (map (curryr expand-proof-term budget) proof))
  ;; get rid of any unnecessary terms
  (define contiguous
    (cons (first expanded) (map remove-front-term (rest expanded))))
  ;; append together the proofs
  (define res
    (apply append contiguous))

  (set-box! budget (- (unbox budget) (length proof)))
  (if (member #f res)
      (list #f)
      res))

(define (egraph-is-equal egraph-data expr goal)
  (define egg-expr (~a (expr->egg-expr expr egraph-data)))
  (define egg-goal (~a (expr->egg-expr goal egraph-data)))
  (egraph_is_equal (egraph-data-egraph-pointer egraph-data) egg-expr egg-goal))

;; returns a flattened list of terms or #f if it failed to expand the proof due to budget
(define (egraph-get-proof egraph-data expr goal)
  (define egg-expr (~a (expr->egg-expr expr egraph-data)))
  (define egg-goal (~a (expr->egg-expr goal egraph-data)))
  (define pointer (egraph_get_proof (egraph-data-egraph-pointer egraph-data) egg-expr egg-goal))
  (define res (cast pointer _pointer _string/utf-8))
  (destroy_string pointer)
  (define env (make-hash))
  (cond
    ;; TODO: sometimes the proof is *super* long and it takes us too long just string-split
    ;; Ideally we would skip the string-splitting
    [(< (string-length res) 10000)
     (define converted
       (for/list ([line (in-list (string-split res "\n"))])
         (egg-expr->expr line egraph-data)))
     (define expanded
       (expand-proof
        converted
        (box (*proof-max-length*))))
     (if (member #f expanded)
         #f
         expanded)]
    [else
     #f]))

(struct egg-add-exn exn:fail ())

;; result function is a function that takes the ids of the nodes
(define (egraph-add-expr eg-data expr)
  (define egg-expr (~a (expr->egg-expr expr eg-data)))
  (egraph_add_expr (egraph-data-egraph-pointer eg-data) egg-expr))

(struct iteration-data (num-nodes num-eclasses time))

(define (convert-iteration-data egraphiters size)
  (cond
    [(> size 0)
     (cons (iteration-data (EGraphIter-numnodes egraphiters)
                           (EGraphIter-numeclasses egraphiters)
                           (EGraphIter-time egraphiters))
           (convert-iteration-data (ptr-add egraphiters 1 _EGraphIter) (- size 1)))]
    [else empty]))

;; runs rules on an egraph
;; can optionally specify an iter limit
(define (egraph-run egraph-data node-limit ffi-rules const-folding? [iter-limit #f])
  (define egraph-ptr (egraph-data-egraph-pointer egraph-data))
  (define-values (iterations length ptr)
    (if iter-limit
        (egraph_run_with_iter_limit egraph-ptr ffi-rules iter-limit node-limit const-folding?)
        (egraph_run egraph-ptr ffi-rules node-limit const-folding?)))
  (define iteration-data (convert-iteration-data iterations length))
  (destroy_egraphiters ptr)
  iteration-data)

(define (egraph-run-rules egg-graph node-limit egg-rules node-ids const-folding? #:limit [iter-limit #f])
  ;; expand rules (will also check cache)
  ;(match-define (list egg-rules ffi-rules canon-names) (map ffi rules))
  (define ffi-rules (map make-ffi-rule egg-rules))
  (define canon-names (map rule-name egg-rules))
  ;; run the rules
  (define iteration-data (egraph-run egg-graph node-limit ffi-rules const-folding? iter-limit))

  ;; get cost statistics
  (let loop ([iter iteration-data] [counter 0] [time 0])
    (unless (null? iter)
      (define cnt (iteration-data-num-nodes (first iter)))
      (define cost (apply + (map (λ (node-id) (egraph-get-cost egg-graph node-id counter)) node-ids)))
      (define new-time (+ time (iteration-data-time (first iter))))
      ;     (timeline-push! 'egraph counter cnt cost new-time)
      (loop (rest iter) (+ counter 1) new-time)))
  ; (timeline-push! 'stop (egraph-stop-reason egg-graph) 1)

  ;; get rule statistics
  (define rule-apps (make-hash))
  (for ([ffi-rule (in-list ffi-rules)] [rule (in-list egg-rules)])
    (define count (egraph-get-times-applied egg-graph ffi-rule))
    (define canon-name (hash-ref canon-names (rule-name rule)))
    (hash-update! rule-apps canon-name (curry + count) count))

  ;  (for ([(name count) (in-hash rule-apps)])
  ;    (when (> count 0) (timeline-push! 'rules (~a name) count)))
  (for-each free-ffi-rule ffi-rules)
  iteration-data)
