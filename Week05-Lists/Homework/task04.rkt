#lang racket

(define (rev-fold xs)
  (foldr (λ (x acc) (+ x (* acc 10))) 0 xs)
  )

(define (rev-lin-rec xs)
  (if (empty? xs)
      0
      (+ (car xs) (* 10 (rev-lin-rec (cdr xs))))
      )
  )

; using a linearly recursive procedure
(= (rev-lin-rec '(1 2 3)) 321)
(= (rev-lin-rec '(1 2 3 4 5 6 7 8 9)) 987654321)

; using folding
(= (rev-fold '(1 2 3)) 321)
(= (rev-fold '(1 2 3 4 5 6 7 8 9)) 987654321)