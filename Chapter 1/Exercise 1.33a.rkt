#lang sicp

(define (filtered-accumulate combiner filter null-value term a next b)
  (cond ((> a b)
         null-value)
        ((filter a)
         (combiner (term a) (filtered-accumulate combiner filter null-value term (next a) next b)))
        (else
         (filtered-accumulate combiner filter null-value term (next a) next b)))) 

(define (even? n) (= (remainder n 2) 0))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))
(define (prime? n)
  (fast-prime? n 100))
(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (check-square (expmod base (/ exp 2) m) m)
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))        
(define (check-square x n)
  (cond ((or (= x 1) (= x (- n 1))) (* x x))
        ((= (remainder (* x x) n) 1) 0)
        (else (* x x))))

;(define (sum a b) (+ a b))
(define (square x) (* x x))

(define (sum-square-primes a b)
  (filtered-accumulate + prime? 0 square a inc b))

(sum-square-primes 2 8) ;87
(sum-square-primes 5 10) ;74