#lang racket

(define (get-list-of-lengths xss)
  (map (λ (x) (length x)) xss)
  )

(define (have-matching-lengths xss yss)
  (if (or (empty? xss) (empty? yss))
      (error "The lists must be non-empty")
      (equal? (get-list-of-lengths xss) (get-list-of-lengths yss))
      )
  )

(equal? (have-matching-lengths '((1 2 3) (4 5 6) (7 8 9)) '((1 4 7) (2 5 8) (3 6 9))) #t)
(equal? (have-matching-lengths '((1 2)) '((1 4 7) (2 5 8))) #f)


