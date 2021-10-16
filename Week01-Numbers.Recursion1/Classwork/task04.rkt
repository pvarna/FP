#lang racket

(define (fact-rec x)
  (if (= x 0)
      1
      (* x (fact-rec (sub1 x)))
      )
  )

(define (fact-iter x)
  (define (helper result left-over)
    (if (= left-over 0)
        result
        (helper (* result left-over) (- left-over 1))
        )
    )
  (helper 1 x)
  )

(= (fact-rec 0) 1)
(= (fact-rec 1) 1)
(= (fact-rec 11) 39916800)


(= (fact-iter 0) 1)
(= (fact-iter 1) 1)
(= (fact-iter 11) 39916800)