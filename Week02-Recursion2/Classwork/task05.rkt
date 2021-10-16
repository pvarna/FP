#lang racket

(require math/number-theory)

(define (sum-divs x)
  (define (helper curr-sum curr-x)
    (cond
      [(= curr-x 1) (+ curr-sum 1)  ]
      [(divides? curr-x x) (helper (+ curr-sum curr-x)(sub1 curr-x))]
      [else (helper curr-sum (sub1 curr-x))]
      )

    )
  (if (< x 1)
      0
      (helper 0 x)
   )
  )


(= (sum-divs 0) 0)
(= (sum-divs 1) 1)
(= (sum-divs 6) 12) ; 1 + 2 + 3 + 6
(= (sum-divs 12345) 19776)