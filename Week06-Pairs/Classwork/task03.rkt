#lang racket

(define (assoc-rec key xs)
  (cond
    [(empty? xs) #f]
    [(equal? key (car (car xs))) (cdr (car xs))]
    [else (assoc-rec key (cdr xs))]
    )
  )

(define (assoc-hop key xs)
  (cdr (car (dropf xs (λ (x) (not (equal? (car x) key))))))
  )

(define (assoc-assoc key xs)
  (cdr (assoc key xs))
  )

; using a recursive process
(equal? (assoc-rec 1 (list '(2 . "Two") '(3 . "Three") '(1 . "One"))) "One")

; using a higher order procedure
(equal? (assoc-hop 1 (list '(2 . "Two") '(3 . "Three") '(1 . "One"))) "One")

; using assoc
(equal? (assoc-assoc 1 (list '(2 . "Two") '(3 . "Three") '(1 . "One"))) "One")