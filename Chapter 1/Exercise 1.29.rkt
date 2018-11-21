#lang sicp

(define (inc x) (+ x 1))
(define (cube x) (* x x x))
(define (odd? n) (= (remainder n 2) 1))
(define (simpson-integral f a b n)
  (define (simpson-term k)
    (* (cond ((or (= k 0) (= k n)) 1)
             ((odd? k) 4)
             (else 2))
       (y k)))
  (define (y k) (f (+ a (* k h))))
  (define h (/ (- b a) n))
  (* (sum simpson-term 0 inc n)
     (/ h 3)))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(simpson-integral cube 0 1 100.0)
(simpson-integral cube 0 1 1000.0)