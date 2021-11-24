#lang racket

(define (get-number-of-trailing-zeroes n)
  (define (helper current-expt result)
    (if (> (expt 5 current-expt) n)
        result
        (helper (add1 current-expt) (+ result (quotient n (expt 5 current-expt))))
        )
    )
  (helper 1 0)
  )

(define (trailing-zeros n)
  (λ (p) (p (get-number-of-trailing-zeroes n)))
  )

(equal? ((trailing-zeros 6) even?) #f)
(equal? ((trailing-zeros 1000) even?) #f)
(equal? ((trailing-zeros 100000) even?) #f)
(equal? ((trailing-zeros 1000000000) even?) #t)