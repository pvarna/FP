#lang racket

(define (remove-first-proc x xs)
  (remq x xs)
  )

(define (remove-first-no-proc x xs)
  (cond
    [(empty? xs) xs]
    [(equal? x (car xs)) (cdr xs)]
    [else (cons (car xs) (remove-first-no-proc x (cdr xs)))]
    )
  )

(equal? (remove-first-no-proc 1 '(1 1 1 2)) '(1 1 2))
(equal? (remove-first-no-proc 1 '(2 5 6)) '(2 5 6))
(equal? (remove-first-no-proc 1 '(1)) '())
(equal? (remove-first-no-proc 1 '(2 1)) '(2))
(equal? (remove-first-no-proc "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN" "RNN"))

; using a predefined procedure
(equal? (remove-first-proc 1 '(1 1 1 2)) '(1 1 2))
(equal? (remove-first-proc 1 '(2 5 6)) '(2 5 6))
(equal? (remove-first-proc 1 '(1)) '())
(equal? (remove-first-proc 1 '(2 1)) '(2))
(equal? (remove-first-proc "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN" "RNN"))