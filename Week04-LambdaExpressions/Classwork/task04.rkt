#lang racket

(define (derive f eps)
  (λ (x) (/ (- (f (+ x eps)) (f x)) eps))
  )

(define (derive-n f n eps)
  (if (= n 1)
      (derive f eps)
      (derive (derive-n f (sub1 n) eps) eps)
      )
  )

(= ((derive-n (λ (x) (* 2 x x x)) 3 1e-3) 2) 12.000015203739167)