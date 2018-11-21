#lang sicp

(define (cont-frac n d k) ;iterative, count from k to 0
  (define (iter i result)
    (let ((n-term (n i))
          (d-term (d i)))
      (if (= i 0)
          result
          (iter (- i 1) (/ n-term (+ d-term result))))))
  (iter (- k 1) (/ (n k) (d k))))

(define pi 3.141592653589793238)
(define (square x) (* x x))

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (- (square x))))
  (define (d i)
    (- (* i 2) 1))
  (cont-frac n d k))

(tan (/ pi 4))
(tan-cf (/ pi 4) 10)
(tan (/ pi 6)) 
(tan-cf (/ pi 6) 10)