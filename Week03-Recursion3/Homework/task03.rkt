#lang racket

(define (get-nth-denominator n)
  (define (helper counter result to-multiply)
    (if (= counter n)
        result
        (helper (add1 counter) (* result to-multiply) (+ 2 to-multiply))
        )
    )
  (helper 0 1 1)
)

(define (calc-series-sum x n)
  (define (helper counter result)
    (if (> counter n)
        result
        (helper (add1 counter) (+ result (/ (* (expt -1 (add1 counter)) (expt 2 (add1 counter)) (expt x counter)) (get-nth-denominator (add1 counter)))))
        )
    )
  (helper 0 0)
  )

(calc-series-sum 1 0) ; -2
(calc-series-sum 1 1) ; -2/3
(calc-series-sum 1 2) ; -1 1/5
(calc-series-sum 1 3) ; -1 1/21
(calc-series-sum 1 4) ; -1 11/135
(calc-series-sum 1 5) ; -1 29/385
(calc-series-sum 1 6) ; -1 937/12285