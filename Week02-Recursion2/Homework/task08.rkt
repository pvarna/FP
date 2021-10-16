#lang racket

(require math/number-theory)

(define (sum-digits n)
  (define (helper sum left-over)
    (if (= left-over 0)
        sum
        (helper (+ sum (remainder left-over 10)) (quotient left-over 10))
        )
    )
  (helper 0 n)
  )

(define (interesting? n)
  (divides? (sum-digits n) n)
  )

(equal? (interesting? 410) #t)