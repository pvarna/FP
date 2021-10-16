#lang racket

(require math/number-theory)

(define (sum-prime-divs-rec n)
  (define (helper current-number)
    (cond
      [(> current-number n) 0]
      [(and (prime? current-number) (divides? current-number n)) (+ current-number (helper (add1 current-number)))]
      [else (helper (add1 current-number))]
      )
    )
  (helper 2)
)

(= (sum-prime-divs-rec 0) 0)
(= (sum-prime-divs-rec 6) 5) ; 2 + 3
(= (sum-prime-divs-rec 18) 5) ; 2 + 3
(= (sum-prime-divs-rec 19) 19)
(= (sum-prime-divs-rec 45136) 53)