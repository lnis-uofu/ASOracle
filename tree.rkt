(struct tree-node (value parent left right color))
(define (insert-node tree item cmp)
  (cond
    [(empty? tree) (tree-node item empty empty empty 'red)]

    [(tree-node-value value left right color)
     (if (cmp item value)
         (tree-node value (insert-node left item cmp) right)
         (tree-node value left (insert-node right item cmp)))]))
