#lang sicp

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (display first-guess)
  (newline)
  (try first-guess))

(fixed-point (lambda (x) (* 0.5 (+ (/ (log 1000) (log x)) x))) 2.0)
;34 steps w/o average damping
;10 steps w/ average damping
