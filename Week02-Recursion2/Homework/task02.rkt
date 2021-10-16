#lang racket

(define (sum-digits-iter n)
  (define (helper sum left-over)
    (if (= left-over 0)
        sum
        (helper (+ sum (remainder left-over 10)) (quotient left-over 10))
        )
    )

  (if (< n 0)
      (error "n was negative")
      (helper 0 n)
  )
)

(= (sum-digits-iter 12345) 15)
(= (sum-digits-iter 123) 6)
; (sum-digits-iter -13) ; error "n was negative"