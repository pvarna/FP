#lang racket

(require racket/trace)

(define (remove-first-occurrence n d)
  (define (helper result left-over found counter)
    (cond
      [(= left-over 0)
            (if found
                result
                (error "There isn't such digit in the number")
                )]
      [(and (not found) (= (remainder left-over 10) d)) (helper result (quotient left-over 10) #t counter)]
      [else (helper (+ (* (remainder left-over 10) (expt 10 counter)) result) (quotient left-over 10) found (add1 counter))]
      )
    )
  (helper 0 n #f 0)
  )

;(trace remove-first-occurrence)

(= (remove-first-occurrence 15365 5) 1536)
(= (remove-first-occurrence 15360 0) 1536)
(= (remove-first-occurrence 15300 0) 1530)
(= (remove-first-occurrence 15365 1) 5365)
(= (remove-first-occurrence 35365 3) 3565)
(= (remove-first-occurrence 1212 1) 122)
(= (remove-first-occurrence 1212 2) 121)
(= (remove-first-occurrence (remove-first-occurrence 1212 1) 1) 22)