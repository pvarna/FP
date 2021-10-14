#lang racket

(define (snail height crawlingUp slidingDown)
  (define (helper counter currentHeight)
    (if (>= currentHeight height)
        counter
        (helper (+ counter 1) (+ (- currentHeight slidingDown) crawlingUp))
        )
    )
  (if (> slidingDown crawlingUp)
      (error "The crawlingUp distance must be longer than the slidingDown distance")
      (helper 1 crawlingUp)
      )
  (helper 1 crawlingUp)
  )

(= (snail 3 2 1) 2)
(= (snail 10 3 1) 5)
(= (snail 10 3 2) 8)
(= (snail 100 20 5) 7)
(= (snail 5 10 3) 1)
; (= (snail 5 3 10) 1)