#lang racket

(define (count-occurrences xs ys)
  (cond
    [(= (length ys) 1) 0]
    [(equal? xs (take ys 2)) (add1 (count-occurrences xs (cdr ys)))]
    [else (count-occurrences xs (cdr ys))]
    )
  )

(= (count-occurrences '(1 5) '(1 5 2 3 1 5 6 7 7 1 5)) 3)
(= (count-occurrences '(5 5) '(5 5 5 3 1 5 6 7 5 5 5)) 4)
(= (count-occurrences '(6 6) '(2 2)) 0)