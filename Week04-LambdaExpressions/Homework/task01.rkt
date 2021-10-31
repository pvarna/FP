#lang racket

(define (big-function f y)
  (λ (x)
    (if (> x (f x))
        y
        (f x)
        )
    )
  )

((big-function (λ (x) (* 2 x)) 100) 50)
((big-function (λ (x) (* 2 x)) 100.236) 500.002)
((big-function identity 1.001) 1.001)