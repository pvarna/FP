#lang racket

(define (set-union first-list second-list)
  (define (helper result left-over)
    (cond
      [(empty? left-over) result]
      [(not (member (car left-over) first-list)) (helper (cons (car left-over) result) (cdr left-over))]
      [else (helper result (cdr left-over))]
      )
    )
   (sort (helper first-list second-list) <)
  )

(equal? (set-union '(1 3 5 7) '(5 7 13)) '(1 3 5 7 13))
(equal? (set-union '(5 7 13) '(1 3 5 7)) '(1 3 5 7 13))