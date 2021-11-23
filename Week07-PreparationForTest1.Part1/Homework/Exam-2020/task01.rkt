#lang racket

(require math/number-theory)

(define (get-list-of-digits n)
  (define (helper result left-over)
    (if (zero? left-over)
        result
        (helper (cons (remainder left-over 10) result) (quotient left-over 10))
        )
    )
  (helper '() n)
  )

(define (get-sum digits p)
  (define (helper result left-over current-p)
    (if (empty? left-over)
        result
        (helper (+ result (expt (car left-over) current-p)) (cdr left-over) (add1 current-p))
        )
    )
  (helper 0 digits p)
  )

(define (dig-pow n p)
  (if (divides? n (get-sum (get-list-of-digits n) p))
      (quotient (get-sum (get-list-of-digits n) p) n)
      -1
      )
  )

(= (dig-pow 89 1) 1)
(= (dig-pow 92 1) -1)
(= (dig-pow 695 2) 2)
(= (dig-pow 46288 3) 51)
