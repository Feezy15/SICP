#lang sicp

(define (good-enough? guess prev-guess)
	(< (abs (- guess prev-guess)) .001))

(define (improve guess x)
	(/ (+ guess (/ x guess)) 2))

(define (sqrt-iter guess prev-guess x)
  (if (good-enough? guess prev-guess)
      guess
      (sqrt-iter (improve guess x) guess x)))	

(define (sqrt x)
	(sqrt-iter 1.0 2.0 x))

(sqrt 536247158)
(sqrt .001)