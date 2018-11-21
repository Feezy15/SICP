#lang sicp
;starting at position (0 0) top of triangle
;rows and columns start at 0
(define (pascal r c)
  (cond ((and (= r 0) (= c 0)) 1)
        ((= r 0) 0) ;account for other degenerate cases on row 0
        (else (+ (pascal (- r 1) (- c 1))
                 (pascal (- r 1) c)))))

(pascal 6 3)
