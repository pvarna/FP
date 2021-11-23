#lang racket

(require math/number-theory)

(define (get-first-half xs)
  (take xs (quotient (length xs) 2))
  )

(define (get-second-half xs)
  (drop xs (quotient (length xs) 2))
  )

(define (shuffle xs)
  (define (helper result left-over-first left-over-second)
    (if (empty? left-over-first)
        result
        (helper (append result (list (car left-over-first)) (list (car left-over-second))) (cdr left-over-first) (cdr left-over-second))
        )
    )
  
  (if (and (not (empty? xs)) (divides? 2 (length xs)))
      (helper '() (get-first-half xs) (get-second-half xs))
      (error "Invalid length")
      )
  )

(equal? (shuffle '(2 5 1 3 4 7)) '(2 3 5 4 1 7))
(equal? (shuffle '(1 2 3 4 4 3 2 1)) '(1 4 2 3 3 2 4 1))
(equal? (shuffle '(1 1 2 2)) '(1 2 1 2))