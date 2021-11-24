#lang racket

(define (get-number-of-occurrences n d)
  (define (helper left-over result)
    (if (< left-over 10)
        (add1 result)
        (helper (remainder left-over 10) (add1 result))
        )
    (cond
      [(zero? left-over) result]
      [(= (remainder left-over 10) d) (helper (quotient left-over 10) (add1 result))]
      [else (helper (quotient left-over 10) result)]
      )
    )
  (if (and (zero? n) (zero? d))
      1
      (helper n 0)
      )
  )

(define (get-distribution n)
  (define (helper left-over result)
    (if (zero? left-over)
        result
        (helper (quotient left-over 10) (cons (cons (remainder left-over 10) (get-number-of-occurrences (* n n) (remainder left-over 10))) result))
        )
    )
  (cond
    [(< n 0) (error "N must be non-negative")]
    [(zero? n) '((0 . 1))]
    [else (sort (remove-duplicates (helper (* n n) '())) (λ (x y) (< (car x) (car y))))]
    )
  )

(equal? (get-distribution 123) '((1 . 2) (2 . 1) (5 . 1) (9 . 1)))
(equal? (get-distribution 0) '((0 . 1)))
;(get-distribution -5)