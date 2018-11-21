#lang sicp
(define (square x) (* x x))

(define (sum-squares x y) (+ (square x) (square y)))

(define (sum-larger-squares x y z) 
  (cond ((and (<= z x) (<= z y)) (sum-squares x y))
        ((and (<= y x) (<= y z)) (sum-squares x z))
        (else (sum-squares y z)))) 

(sum-larger-squares 1 2 3)
(sum-larger-squares 1 1 1)
(sum-larger-squares 1 2 2)
(sum-larger-squares 1 1 2)