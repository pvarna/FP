#lang racket

(define (growing-plant upSpeed downSpeed desiredHeight)
  (define (helper counter currentHeight)
    (if (>= currentHeight desiredHeight)
        counter
        (helper (+ counter 1) (+ upSpeed (- currentHeight downSpeed)))
        )
    )
  (if (> downSpeed upSpeed)
      (error "The upSpeed must be higher that the downSpeed")
      (helper 1 upSpeed)
      )
  )

(= (growing-plant 5 2 5) 1)
(= (growing-plant 5 2 6) 2)
(= (growing-plant 10 9 4) 1)
(= (growing-plant 100 10 910) 10) ; upSpeed=100, downSpeed=10, desiredHeight=910
; (= (growing-plant 10 100 910) 10)