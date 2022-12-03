(require "expander.rkt")

;; Optimizations:
;; all summations or integration should be wrapped in either negation or double negation.
;; push negations up from children to summation.
;; if more children are negation, negate the summation and distribute negation. otherwise double negate.
