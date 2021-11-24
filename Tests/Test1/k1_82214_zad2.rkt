#lang racket

(define (get-sorted-list-of-lengths xss)
  (sort (map (λ (xs) (length xs)) xss) <)
  )

(define (is-list-valid xss)
  (and (not (empty? xss)) (not (ormap empty? xss)))
  )

(define (get-missing-length xss)
  (define (helper left-over)
    (cond
      [(= (length left-over) 1) (error "Wrong list of lists")]
      [(not (= (add1 (car left-over)) (cadr left-over))) (add1 (car left-over))]
      [else (helper (cdr left-over))]
      )
    )
  (if (is-list-valid xss)
      (helper (get-sorted-list-of-lengths xss))
      (error "Empty list!")
      )
  )

(= (get-missing-length '((1 2) (4 5 1 1) (1) (5 6 7 8 9))) 3)
(= (get-missing-length '(("a", "a", "a") ("a", "a") ("a", "a", "a","a") ("a") ("a", "a", "a", "a", "a", "a"))) 5)

;(get-missing-length '())
;(get-missing-length '((1 2) () (5 6 7 8)))