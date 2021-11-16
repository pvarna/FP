#lang racket

(define (get-number-of-greater-elements x xs)
  (cond
    [(empty? xs) 0]
    [(> (car xs) x) (add1 (get-number-of-greater-elements x (cdr xs)))]
    [else (get-number-of-greater-elements x (cdr xs))]
    )
  )

(define (push-back x xs)
  (append xs (list x))
  )

(define (num-bigger-elements xs)
  (define (helper result left-over)
    (if (empty? left-over)
        result
        (helper (push-back (cons (car left-over) (get-number-of-greater-elements (car left-over) xs)) result) (cdr left-over))
        )
    )
  (helper '() xs)
  )

(equal? (num-bigger-elements '(5 6 3 4)) '((5 . 1) (6 . 0) (3 . 3) (4 . 2)))
(equal? (num-bigger-elements '(1 1 1)) '((1 . 0) (1 . 0) (1 . 0)))
