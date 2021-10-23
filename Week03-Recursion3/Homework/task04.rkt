#lang racket

(require math/number-theory)
(require racket/trace)

(define (sum-digits n)
  (if (< n 10)
      n
      (+ (remainder n 10) (sum-digits (quotient n 10)))
      )
)

(define (sum-divisible-numbers start finish k)
  (if (> start finish)
      (sum-divisible-numbers finish start k)
      (cond
        [(= start finish)
         (if (divides? k (sum-digits start))
             (sum-digits start)
             0
             )]
        [(divides? k (sum-digits start)) (+ start (sum-divisible-numbers (add1 start) finish k))]
        [else (sum-divisible-numbers (add1 start) finish k)]
        )
      )
)

;(trace sum-divisible-numbers)

(= (sum-divisible-numbers 0 10 5) 5)
(= (sum-divisible-numbers 0 100 5) 990)
(= (sum-divisible-numbers 100 0 5) 990)