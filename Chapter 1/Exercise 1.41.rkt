#lang sicp

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))

((double inc) 6) ;8
(((double (double double)) inc) 5) ;21 idk how to trace
