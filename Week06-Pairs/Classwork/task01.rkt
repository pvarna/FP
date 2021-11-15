#lang racket

(define (my-reverse-iter xs)
  (define (helper result left-over)
    (if (empty? left-over)
        result
        (helper (cons (car left-over) result) (cdr left-over))
        )
    )
  (helper '() xs)
  )

(equal? (my-reverse-iter '(1 2 3 4 5)) '(5 4 3 2 1))
(equal? (my-reverse-iter '(-3 8 0 5 3)) '(3 5 0 8 -3))