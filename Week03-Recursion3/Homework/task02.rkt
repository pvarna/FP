#lang racket

(require racket/trace)

(define (find-max n)
  (define (helper current-n max)
    (cond
      [(zero? current-n) max]
      [(> (remainder current-n 10) max) (helper (quotient current-n 10) (remainder current-n 10))]
      [else (helper (quotient current-n 10) max)]
      )
  )
  (helper (quotient n 10) (remainder n 10))
)

(define (remove-first-occurrence n d)
  (define (helper result left-over found counter)
    (cond
      ((and (= left-over 0) (not found)) (error "There isn't such digit in the number"))
      ((and (= left-over 0) found) result)
      ((and (not found) (= (remainder left-over 10) d)) (helper result (quotient left-over 10) #t counter))
      (else (helper (+ (* (remainder left-over 10) (expt 10 counter)) result) (quotient left-over 10) found (add1 counter)))
      )
    )
  (helper 0 n #f 0)
  )

(define (count-digits-rec n)
  (cond
    [(< n 0) (error "n was negative")]
    [(< n 10) 1]
    [else (+ 1 (count-digits-rec (quotient n 10)))]
    )
  )

(define (sort-n n)
  (define (helper result left-over counter count-digits)
    (cond
      [(and (= left-over 0) (= counter count-digits)) result]
      [(and (= left-over 0) (< counter count-digits)) (* result (expt 10 (- count-digits counter)))]
      [else (helper (+ (* result 10) (find-max left-over)) (remove-first-occurrence left-over (find-max left-over)) (add1 counter) count-digits)]
      )
    )
  (helper 0 n 0 (count-digits-rec n))
)

;(trace sort-n)

(= (sort-n 1714) 7411)
(= (sort-n 123450) 543210)
(= (sort-n 123405) 543210)
(= (sort-n 123045) 543210)
(= (sort-n 120345) 543210)
(= (sort-n 102345) 543210)
(= (sort-n 8910) 9810)
(= (sort-n 321) 321)
(= (sort-n 29210) 92210)
(= (sort-n 1230) 3210)
(= (sort-n 55345) 55543)
(= (sort-n 14752) 75421)
(= (sort-n 329450) 954320)
(= (sort-n 9125) 9521)