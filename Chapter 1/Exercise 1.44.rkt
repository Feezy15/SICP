#lang sicp

(define (smooth f dx)
  (lambda (x) (/ (+ (f x)
                    (f (- x dx))
                    (f (+ x dx)))
                 3)))

(define (n-fold-smooth f dx n)
  (repeated (smooth f dx) n))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose (repeated f (- n 1)) f)))

(define pi 3.1415926535897984626)

(sin (/ pi 2))
((smooth sin 0.7) (/ pi 2))
((n-fold-smooth sin 0.7 2) (/ pi 2))
((n-fold-smooth sin 0.7 64) (/ pi 2))
((n-fold-smooth sin 0.7 256) (/ pi 2))
