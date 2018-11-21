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

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  (fast-prime? n 100))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))        

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x) (* x x))
  
(define (search-for-primes a b)
  (cond ((> a b) )
        ((even? a) (search-for-primes (+ a 1) b))
        (else (timed-prime-test a)
              (search-for-primes (+ a 2) b))))

(search-for-primes 10000000 10000061) ;10^7
(search-for-primes 100000000 100000057) ;10^8
(search-for-primes 1000000000 1000000063) ;10^9