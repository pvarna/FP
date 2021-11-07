#lang racket

(require math/number-theory)

(define (factorize num)
  (define (helper left-over result current-divisor)
    (cond
      [(= left-over 1) result]
      [(divides? current-divisor left-over) (helper (quotient left-over current-divisor) (cons current-divisor result) current-divisor)]
      [else (helper left-over result (add1 current-divisor))]
      )
    )
  (reverse (helper num '() 2))
)

(equal? (factorize 2) '(2))
(equal? (factorize 6) '(2 3))
(equal? (factorize 13) '(13))
(equal? (factorize 123) '(3 41))
(equal? (factorize 152) '(2 2 2 19))
(equal? (factorize 12356498) '(2 7 11 19 41 103))