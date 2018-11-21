#lang sicp

(define (even? n) (= (remainder n 2) 0))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x) (* x x))

(define (search-for-primes a b)
  (cond ((> a b) )
        ((even? a) (search-for-primes (+ a 1) b))
        (else (timed-prime-test a)
              (search-for-primes (+ a 2) b))))

(search-for-primes 10000000000 10000000061) ;10^10
(search-for-primes 100000000000 100000000057) ;10^11
(search-for-primes 1000000000000 1000000000063) ;10^12