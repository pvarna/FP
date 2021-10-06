#lang racket

(define (fib-rec x)
  (cond
    [(< x 0) (error "x is negative")]
    [(= x 0) 0]
    [(= x 1) 1]
    [else (+ (fib-rec (- x 1)) (fib-rec (- x 2)))]
    )
  )

(define (fib-iter x)
  (define (helper prev2 prev1 left-over)
    (cond
      [(< left-over 0) (error "x is negative")]
      [(= left-over 0) prev2]
      [(= left-over 1) prev1]
      [else (helper prev1 (+ prev1 prev2) (- left-over 1))]
      )
    )
  (helper 0 1 x)
  )

(= (fib-rec 11) 89)

(= (fib-iter 11) 89)