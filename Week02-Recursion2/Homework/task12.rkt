#lang racket

(define (find-single-sum a b n)
  (define (helper sum current-n)
    (if (> current-n n)
        sum
        (helper (+ sum (* (expt 2 (- current-n 1)) b)) (add1 current-n))
        )
    )
  (helper a 1)
  )

(define (find-sum a b n)
  (if (< n 3)
      (error "n must be bigger than 3")
      (+ (find-single-sum a b n) (find-single-sum a b (sub1 n)) (find-single-sum a b (- n 2)))
      )
  )

(= (find-sum 0 2 10) 3578) ; 510 + 1022 + 2046
(= (find-sum 5 3 5) 174) ; 26 + 50 + 98
