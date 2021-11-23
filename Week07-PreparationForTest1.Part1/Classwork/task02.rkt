#lang racket

(require math/number-theory)

(define (sum-digit-divisors n)
  (define (helper left-over result)
    (cond
      [(zero? left-over) result]
      [(divides? (remainder left-over 10) n) (helper (quotient left-over 10) (+ result (remainder left-over 10)))]
      [else (helper (quotient left-over 10) result)]
      )
    )
  (helper n 0)
  )

(= (sum-digit-divisors 1) 1)
(= (sum-digit-divisors 28) 2)
(= (sum-digit-divisors 32) 2)
(= (sum-digit-divisors 29) 0)
(= (sum-digit-divisors 34) 0)
(= (sum-digit-divisors 1048) 13)
