#lang racket

(define (rev number)
  (define (helper result left-over)
    (if (= left-over 0)
        result
        (helper (+ (* result 10) (remainder left-over 10)) (quotient left-over 10))
        )
    )
  (if (< number 0)
      (error "The number must be non-negative")
      (helper (remainder number 10) (quotient number 10))
      )
  )

(= (rev 1) 1)
(= (rev 123) 321)
(= (rev 987654321) 123456789)
(= (rev 0) 0)
(= (rev 6587102) 2017856)