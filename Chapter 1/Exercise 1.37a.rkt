#lang sicp

(define (cont-frac n d k) ;recursive, count from 1 to k
      (define (frac i)
        (let ((n-term (n i))
              (d-term (d i)))
          (if (= i k)
              (/ n-term d-term)
              (/ n-term (+ d-term (frac (+ i 1)))))))
  (frac 1))
            

(cont-frac (lambda (i) 1.0) ;approximate 1/phi
           (lambda (i) 1.0) ;0.61803398875
           10) ;requires 10 steps for approximation within 4 dec. places
  