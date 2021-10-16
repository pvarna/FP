#lang racket

(require math/number-theory)
(require racket/trace)

(define (sum-digits-rec n)
  (cond
    [(not (positive? n)) (error "n was not positive")]
    [(< n 10) n]
    [else (+ (remainder n 10) (sum-digits-rec (quotient n 10)))]
  )
)

(= (sum-digits-rec 123) 6)
(= (sum-digits-rec 12345) 15)
;(sum-digits-rec -12345)