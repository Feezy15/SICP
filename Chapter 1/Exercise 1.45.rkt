#lang sicp

(define (repeated f n)
  (if (= n 1)
      f
      (compose (repeated f (- n 1)) f)))

(define (compose f g)
  (lambda (x) (f (g x))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (average x y) (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (expt x n)
  (define (iter i total)
    (if (= i 0)
        total
        (iter (- i 1) (* x total))))
  (iter n 1))

(define (nth-root-fixed-point n x)
  (define (iter i) ;inefficient
    (if (< n (expt 2 i))
        (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1)))) (repeated average-damp (- i 1)) 1.0)
        (iter (+ i 1))))
  (iter 2))

(define (nth-root n x)
  (nth-root-fixed-point n x))

;experiments
;(fixed-point-of-transform (lambda (y) (/ 14 y)) average-damp 1.0) ;square root of 4
;(fixed-point-of-transform (lambda (y) (/ 8 (* y y))) average-damp 1.0) ;cube root of 8
;(fixed-point-of-transform (lambda (y) (/ 16 (* y y y))) (repeated average-damp 2) 1.0) ;4th root of 16
;(fixed-point-of-transform (lambda (y) (/ 32 (* y y y y))) (repeated average-damp 2) 1.0) ;5th root of 32
;(fixed-point-of-transform (lambda (y) (/ 64 (* y y y y y))) (repeated average-damp 2) 1.0) ;6th root of 64
;(fixed-point-of-transform (lambda (y) (/ 128 (* y y y y y y))) (repeated average-damp 2) 1.0) ;7th root of 128
;(fixed-point-of-transform (lambda (y) (/ 256 (* y y y y y y y))) (repeated average-damp 3) 1.0) ;8th root of 256
;(fixed-point-of-transform (lambda (y) (/ 4096 (* y y y y y y y y y y y))) (repeated average-damp 3) 1.0) ;12th root of 4096
;(fixed-point-of-transform (lambda (y) (/ 32768 (* y y y y y y y y y y y y y y))) (repeated average-damp 3) 1.0) ;15th root of 32768
;(fixed-point-of-transform (lambda (y) (/ 65536 (* y y y y y y y y y y y y y y y))) (repeated average-damp 4) 1.0) ;16th root of 65536

(nth-root 16 65536) ;16th root of 65536 - 2
(nth-root 4 1296) 6