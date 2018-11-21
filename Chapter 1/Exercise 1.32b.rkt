#lang sicp

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

;(define (product a b) (* a b))
(define (identity x) x)
(define (inc x) (+ x 1))
(define (square x) (* x x))

(define (factorial n)
  (accumulate * 1 identity 1 inc n))

(define (pi-sum a b)
  (define (pi-next x) (+ x 2))
  (define (pi-term x)
    (cond  ((= x 1)
            (/ (+ x 1) (square x)))
           ((or (= x (- b 1)) (= x b))
            (/ (square (+ x 1)) ( *(square x) (+ x 2))))
           (else (square (/ (+ x 1) x))))) 
  (* 4 (accumulate * 1 pi-term a pi-next b)))

(factorial 7) ;5040
(pi-sum 1.0 10000000) ;3.14159265