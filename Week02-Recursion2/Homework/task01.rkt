#lang racket

(require racket/trace)

(define (count-digits-rec n)
  (cond
    [(< n 0) (error "n was negative")]
    [(< n 10) 1]
    [else (+ 1 (count-digits-rec (quotient n 10)))]
    )
  )

(define (count-digits-iter n)
  (define (helper counter left-over)
    (if (< left-over 10)
        counter
        (helper (add1 counter) (quotient left-over 10))
        )
        
    )

  (if (< n 0)
      (error "n was negative")
      (helper 1 n)
      )
  )

(= (count-digits-iter 12345) 5)
(= (count-digits-iter 123) 3)

(= (count-digits-rec 12345) 5)
(= (count-digits-rec 123) 3)
(count-digits-iter -13) ; error "n was negative"