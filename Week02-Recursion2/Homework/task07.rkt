#lang racket

(define (count-occurences n d)
  (define (helper counter left-over)
    (cond
      [(= left-over 0) counter]
      [(= (remainder left-over 10) d) (helper (add1 counter) (quotient left-over 10))]
      [else (helper counter (quotient left-over 10))]
      )
    )

  (if (> n 0)
      (helper 0 n)
      (error "Negative number!")
      )
  )

(= (count-occurences 121 1) 2)
(count-occurences -121 1) ; error "Negative number!"