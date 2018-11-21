#lang sicp

(define (even? n) (= (remainder n 2) 0))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n times) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  (fast-prime? n (- n 1)))

(define (fermat-test n a)
    (= (expmod a n n) a))

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))        

;first few Carmichael numbers 561, 1105, 1729, 2465,2821, 6601
(prime? 561)
(prime? 1105)
(prime? 1729)
(prime? 2465)
(prime? 2821)
(prime? 6601)
(prime? 6602) ;test