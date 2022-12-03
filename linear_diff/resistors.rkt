#lang racket
(require memo)

(provide eia-e96 eia-e48 eia-e24 eia-e12
         divider-closest resistor-closest feedback-closest)

(define (flatmap proc seq)
  (foldr append '() (map proc seq)))

(define/memoize (spread sequence precision)
  (list->vector
   (sort (flatmap (lambda (n)
                    (map (lambda (m) (list (/ m n) m n precision)) sequence))
                  sequence)
         (lambda (a b) (< (first a) (first b))))))

;; EIA E96 1% standard values
(define eia-e96 '(10.0 	10.2 	10.5 	10.7 	11.0 	11.3 	11.5 	11.8 	12.1 	12.4 	12.7 	13.0
                  13.3 	13.7 	14.0 	14.3 	14.7 	15.0 	15.4 	15.8 	16.2 	16.5 	16.9 	17.4
                  17.8 	18.2 	18.7 	19.1 	19.6 	20.0 	20.5 	21.0 	21.5 	22.1 	22.6 	23.2
                  23.7 	24.3 	24.9 	25.5 	26.1 	26.7 	27.4 	28.0 	28.7 	29.4 	30.1 	30.9
                  31.6 	32.4 	33.2 	34.0 	34.8 	35.7 	36.5 	37.4 	38.3 	39.2 	40.2 	41.2
                  42.2 	43.2 	44.2 	45.3 	46.4 	47.5 	48.7 	49.9 	51.1 	52.3 	53.6 	54.9
                  56.2 	57.6 	59.0 	60.4 	61.9 	63.4 	64.9 	66.5 	68.1 	69.8 	71.5 	73.2
                  75.0 	76.8 	78.7 	80.6 	82.5 	84.5 	86.6 	88.7 	90.9 	93.1 	95.3 	97.6))

;; EIA E48 2% standard values
(define eia-e48 '(10.0 	10.5 	11.0 	11.5 	12.1 	12.7 	13.3 	14.0 	14.7 	15.4 	16.2 	16.9
                  17.8 	18.7 	19.6 	20.5 	21.5 	22.6 	23.7 	24.9 	26.1 	27.4 	28.7 	30.1
                  31.6 	33.2 	34.8 	36.5 	38.3 	40.2 	42.2 	44.2 	46.4 	48.7 	51.1 	53.6
                  56.2 	59.0 	61.9 	64.9 	68.1 	71.5 	75.0 	78.7 	82.5 	86.6 	90.9 	95.3))

;; EIA E24 5% standard values
(define eia-e24 '(10.0 	11.0 	12.0 	13.0 	15.0 	16.0 	18.0 	20.0 	22.0 	24.0 	27.0 	30.0
                  33.0 	36.0 	39.0 	43.0 	47.0 	51.0 	56.0 	62.0 	68.0 	75.0 	82.0 	91.0))

;; EIA E12 10% standard values
(define eia-e12 '(10.0 	12.0 	15.0 	18.0 	22.0 	27.0 	33.0 	39.0 	47.0 	56.0 	68.0 	82.0))

(define (closer n a b)
  (< (abs (- a n)) (abs (- b n))))

;; Pick the closest resistor feedback pair to the given gain
(define (divider-closest eia number)
  (let*
      ([eia (spread eia)]
       [order (floor (log number 10))]
       [normal (/ number (exp 10 order))]
       [closest (sort eia (lambda (a b) (closer normal (first a) (first b))))])
      (cons order (first closest))))

;; Pick the closest standard resistor to the given resistance
(define (resistor-closest eia number)
  (let*
      ([order (floor (log number 10))]
      [normal (/ number (exp 10 order))]
      [closest (sort eia (curry closer normal))])
      (list (first closest) order)))

;; Calculate the closest standard resistor summing amplifier input
;; resistor given the feedback resistor and the target gain.
(define (feedback-closest eia feedback gain)
  (let*
       ([order (floor (log gain 10))]
       [normal (/ gain (exp 10 order))]
       [approx (lambda (x) (abs (- gain (/ feedback x))))]
       [target (lambda (a b) (< (approx a) (approx b)))]
       [closest (sort eia target)])
       (first closest)))
