#lang racket

(define (pow-rec x n)
  (cond
    [(<= n 0) (error "n was not positive")]
    [(= n 1) x]
    [else (* x (pow-rec x (sub1 n)))]
    )
  )

(define (pow-iter x n)
  (define (helper result current-n)
    (if (= current-n 1)
        result
        (helper (* result x) (sub1 current-n))
        )
    )
  (helper x n)
  )

(= (pow-rec 2 5) 32)
(= (pow-rec 15 3) 3375)

(= (pow-iter 2 5) 32)
(= (pow-iter 15 3) 3375)
