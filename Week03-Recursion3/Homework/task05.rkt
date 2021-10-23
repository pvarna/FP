#lang racket

(define (p n)
  (if (= n 1)
      1
      (+ (* 3 n) -2 (p (sub1 n)))
      )
  )

(= (p 1) 1)
(= (p 2) 5)
(= (p 3) 12)
(= (p 4) 22)
(= (p 5) 35)
(= (p 6) 51)

; Source: https://math.stackexchange.com/questions/453562/formula-for-pentagonal-numbers/453565