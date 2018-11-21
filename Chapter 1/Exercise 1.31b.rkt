#lang sicp

(define (inc x) (+ x 1))
(define (identity x) x)
(define (square x) (* x x))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(define (pi-sum a b)
  (define (pi-next x) (+ x 2))
  (define (pi-term x)
    (cond  ((= x 1)
            (/ (+ x 1) (square x)))
           ((or (= x (- b 1)) (= x b))
            (/ (square (+ x 1)) ( *(square x) (+ x 2))))
           (else (square (/ (+ x 1) x))))) 
  (* 4 (product pi-term a pi-next b)))

(factorial 5)
(pi-sum 1.0 1000000)
