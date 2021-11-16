#lang racket

(require racket/trace)

(define (satisfies-all x predicates)
  (cond
    [(empty? predicates) #t]
    [(not ((car predicates) x)) #f]
    [else (satisfies-all x (cdr predicates))]
    )
  )

(define (push-back x xs)
  (append xs (list x))
  )

(define (where xs predicates)
  (define (helper result left-over)
    (cond
      [(empty? left-over) result]
      [(satisfies-all (car left-over) predicates) (helper (push-back (car left-over) result) (cdr left-over))]
      [else (helper result (cdr left-over))]
      )
    )
  (helper '() xs)
  )


(equal? (where '(3 4 5 6 7 8 9 10) (list even? (lambda (x) (> x 5)))) '(6 8 10)) ; all even numbers greater than 5
(equal? (where '(3 4 5 7) (list even? (lambda (x) (> x 5)))) '()) ; no numbers are even and greater than 5
(equal? (where '() (list odd? (λ (x) (> x 3)) (λ (x) (< x 20)))) '())
(equal? (where '(1 2 3 4 5 6 7 8 9 10 11 13 15) (list odd? (λ (x) (> x 3)) (λ (x) (< x 20)))) '(5 7 9 11 13 15))