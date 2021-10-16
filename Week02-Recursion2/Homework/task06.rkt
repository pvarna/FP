#lang racket

(require math/number-theory)

(define (contains-digit? number d)
  (define (helper left-over)
    (cond
      [(= left-over 0) #f]
      [(= d (remainder left-over 10)) #t]
      [else (helper (quotient left-over 10))]
      )
    )
  (helper number)
  )

(define (sum-special-primes n d)
  (define (helper sum current-count current-number)
    (cond
      [(= current-count n) sum]
      [(and (prime? current-number) (contains-digit? current-number d)) (helper (+ sum current-number) (add1 current-count) (add1 current-number))]
      [else (helper sum current-count (add1 current-number))]
      )
    )
  (helper 0 0 2)
  )

(= (sum-special-primes 5 2) 392)
(= (sum-special-primes 5 3) 107)
(= (sum-special-primes 10 3) 462)