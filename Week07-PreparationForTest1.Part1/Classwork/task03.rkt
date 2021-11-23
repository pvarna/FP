#lang racket

(require math/number-theory)

(define (sum-digit-divisors x)
  (define (helper current)
    (cond
      [(zero? current) 0]
      [(divides? (remainder current 10) x) (+ (remainder current 10) (helper (quotient current 10)))]
      [else (helper (quotient current 10))]
      )
    )
  (helper x)
  )

(define (push-back x xs)
  (append xs (list x))
  )

(define (get-partial-pairs y xs)
  (define (helper result left-over)
    (cond
      [(empty? left-over) result]
      [(= y (car left-over)) (helper result (cdr left-over))]
      [else (helper (push-back (cons y (car left-over)) result) (cdr left-over))]
      )
    )
  (helper '() xs)
  )

(define (get-cartesian-product xs)
  (define (helper result left-over)
    (if (empty? left-over)
        result
        (helper (append result (get-partial-pairs (car left-over) xs)) (cdr left-over))
        )
    )
  (helper '() xs)
  )

(define (same-sum a b)
  (define (helper left-over counter)
    (cond
      [(empty? left-over) (quotient counter 2)]
      [(= (sum-digit-divisors (caar left-over)) (sum-digit-divisors (cdar left-over))) (helper (cdr left-over) (add1 counter))]
      [else (helper (cdr left-over) counter)]
      )
    )
  (helper (get-cartesian-product (range a (add1 b))) 0)
  )
  
(= (same-sum 28 35) 2) ; the pairs are (28,32) and (29,34)