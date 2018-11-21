#lang sicp

(define (cont-frac n d k) ;iterative, count from k to 0
  (define (iter i result)
    (let ((n-term (n i))
          (d-term (d i)))
      (if (= i 0)
          result
          (iter (- i 1) (/ n-term (+ d-term result))))))
  (iter (- k 1) (/ (n k) (d k))))
        
(define (d i) ;series
  (if (= (remainder (+ i 1) 3) 0)
      (* (/ (+ i 1) 3) 2)
      1))

(define (approx-e k)
  (+ 2 (cont-frac (lambda (i) 1.0) d k)))

(approx-e 10)
           

  