#lang sicp

(define (cont-frac n d k) ;iterative, count from k to 0
  (define (iter i result)
    (let ((n-term (n i))
          (d-term (d i)))
      (if (= i 0)
          result
          (iter (- i 1) (/ n-term (+ d-term result))))))
  (iter (- k 1) (/ (n k) (d k))))

(cont-frac (lambda (i) 1.0) ;approximate 1/phi
           (lambda (i) 1.0) ;0.61803398875
           10) ;requires 10 steps for approximation within 4 dec. places
  
           

  