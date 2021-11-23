#lang racket

(define (are-first-n-elements-zeroes xs n)
  (andmap zero? (take xs n))
  )

(define (triangular? mat)
  (define (helper left-over counter)
    (cond
      [(empty? left-over) #t]
      [(are-first-n-elements-zeroes (car left-over) counter) (helper (cdr left-over) (add1 counter))]
      [else #f]
      )
    )
  (helper mat 0)
  )

(triangular? '((1 2 3)
               (0 5 6)
               (0 0 9)))

(not (triangular? '((0 2 3)
               (0 0 6)
               (1 0 0))))

(not (triangular? '((1 2 3)
               (1 5 6)
               (0 0 9))))

(triangular? '((1 2 3 4)
               (0 5 6 7)
               (0 0 8 9)
               (0 0 0 9))) 