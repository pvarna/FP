#lang racket

(define (insert-at x position xs)
  (define (helper current-index result added)
    (cond
      [(= current-index (length xs)) result]
      [(and (= current-index position) (not added)) (helper current-index (cons x result) #t)]
      [else (helper (add1 current-index) (cons (list-ref xs current-index) result) added)]
      )
    )
  (cond
    [(or (< position 0) (> position (length xs))) (error "Invalid index")]
    [(= position (length xs)) (reverse (cons x xs))]
    [else (reverse (helper 0 '() #f))]
    )
  )

(equal? (insert-at 1 0 '()) '(1))
(equal? (insert-at 1 0 '(2)) '(1 2))
(equal? (insert-at 10 1 '(1 2 3)) '(1 10 2 3))