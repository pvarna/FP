#lang racket

(require racket/trace)

(define (occurrences-of-d-in-n d n)
  (define (helper result left-over)
    (cond
      [(= left-over 0) result]
      [(= d (remainder left-over 10)) (helper (add1 result) (quotient left-over 10))]
      [else (helper result (quotient left-over 10))]
      )
    )
  (helper 0 n)
  )

(define (sum-digits n)
  (define (helper result left-over)
    (if (= left-over 0)
        result
        (helper (+ result (remainder left-over 10)) (quotient left-over 10))
        )
    )
  (helper 0 n)
)

(define (sum-counts-iter x d)
  (define (helper result current-number)
    (if (> current-number x)
        result
        (helper (+ result (occurrences-of-d-in-n d current-number)) (add1 current-number))
        )
    )
  (cond
    [(or (< d 0) (> d 9)) (error "d must be a 1-digit non-negative number")]
    [(< x 1) (error "x must be greater than 0")]
    [else (sum-digits (helper 0 1))]
    )
)

(sum-counts-iter 1 1) ; -> 1
(sum-counts-iter 5123 1) ; -> 19
(sum-counts-iter 1234 8) ; -> 10
(sum-counts-iter 5555 5) ; -> 10
(sum-counts-iter 65432 6) ; -> 11
(sum-counts-iter 70000 1) ; -> 11
(sum-counts-iter 123321 1) ; -> 29

(define (count-digits n)
  (if (< n 10)
      1
      (+ 1 (count-digits (quotient n 10)))
      )
  )

(define (add-ones n)
  (define (helper result left-over counter-digits)
    (if (= left-over 0)
        result
        (helper (+ (* (add1 (remainder left-over 10)) (expt 10 counter-digits)) result) (quotient left-over 10) (+ counter-digits (count-digits (add1 (remainder left-over 10)))))
        )
    )
  (if (> n 0)
      (helper 0 n 0)
      (error "n must be greater than 0")
      )
)

(add-ones 123) ; -> 234
(add-ones 193) ; -> 2104
(add-ones 998) ; -> 10109
(add-ones 9999) ; -> 10101010