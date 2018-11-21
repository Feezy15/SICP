#lang sicp

(define (iterative-improve good-enough? improve)
  (define (help x)
    (if (good-enough? x)
        x
        (help (improve x))))
  help)

(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))

(define (sqrt x)
  ((iterative-improve (lambda (guess)
                         (< (abs (- (square guess) x)) 0.001))
                      (lambda (guess)
                        (average guess (/ x guess))))
   1.0))

(define (fixed-point f first-guess)
  ((iterative-improve (lambda (guess)
                       (< (abs (- guess (f guess))) 0.00001))
                     (lambda (guess)
                       (f guess)))
  first-guess))

(sqrt 4)
(sqrt 25)
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)