#lang racket

(define (remove-all-no-proc x xs)
  (define (helper left-over result)
    (cond
      [(empty? left-over) result]
      [(equal? x (car left-over)) (helper (cdr left-over) result)]
      [else (helper (cdr left-over) (cons (car left-over) result))]
      )
    )
  (reverse (helper xs '()))
  )

(define (remove-all-proc x xs)
  (remq* (list x) xs)
  )

; using a predefined procedure
(equal? (remove-all-proc 1 '(1 1 1 2)) '(2))
(equal? (remove-all-proc 1 '(2 5 6)) '(2 5 6))
(equal? (remove-all-proc 1 '(1)) '())
(equal? (remove-all-proc 1 '(1 2 1 1)) '(2))
(equal? (remove-all-proc "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN"))

; without using a predefined procedure
(equal? (remove-all-no-proc 1 '(1 1 1 2)) '(2))
(equal? (remove-all-no-proc 1 '(2 5 6)) '(2 5 6))
(equal? (remove-all-no-proc 1 '(1)) '())
(equal? (remove-all-no-proc 1 '(1 2 1 1)) '(2))
(equal? (remove-all-no-proc "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN"))