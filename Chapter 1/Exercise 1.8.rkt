#lang sicp

(define (cbrt-iter guess prev-guess x)
	(if (good-enough? guess prev-guess)
		guess
		(cbrt-iter (improve guess x) guess x)))

(define (good-enough? guess prev-guess)
	(< (abs (- guess prev-guess)) (* guess 0.001)))
(define (improve guess x)
	(/ (+ (/ x (square guess)) (* 2 guess))
	   3))

(define (square x) (* x x))


(define (cbrt x) 
	(cbrt-iter 1.0 2.0 x))

(cbrt 3)
(cbrt 65)
(cbrt 125)