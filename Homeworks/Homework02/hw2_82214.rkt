#lang racket

(define (push-back x xs)
  (append xs (list x))
  )

(define (get-all-airports-from start flights)
  (define (helper result left-over)
    (cond
      [(empty? left-over) result]
      [(equal? start (car (car left-over))) (helper (push-back (cdr (car left-over)) result) (cdr left-over))]
      [else (helper result (cdr left-over))]
      )
    )
  (sort (helper '() flights) string<?)
  )

(define (itinerary flights)
  (λ (start)
    (define (helper result left-over current-start connected-airports)
      (cond
        [(empty? left-over) result]
        [(empty? connected-airports) (error "No such itinerary!")]
        [else (helper (push-back (car connected-airports) result) (remove (cons current-start (car connected-airports)) left-over) (car connected-airports) (get-all-airports-from (car connected-airports) (remove (cons current-start (car connected-airports)) left-over)))]
        )
      )
    (helper (list start) flights start (get-all-airports-from start flights))
    )
  )

(equal? ((itinerary '(("SFO" . "HKO") ("YYZ" . "SFO") ("YUL" . "YYZ") ("HKO" . "ORD"))) "YUL") '("YUL" "YYZ" "SFO" "HKO" "ORD"))
(equal? ((itinerary '(("A" . "B") ("A" . "C") ("B" . "C") ("C" . "A"))) "A") '("A" "B" "C" "A" "C"))
;((itinerary '(("SFO" . "COM") ("COM" . "YYZ"))) "COM") ;"No such itinerary!"

(define (get-top-or-bottom-padding x length)
  (if (= length 1)
      (list x)
      (append (get-top-or-bottom-padding x (sub1 length)) (list x))
      )
  )

(define (get-left-and-right-padding x middle)
  (append (list x) middle (list x))
  )

(define (pad xs)
  (λ (x)
    (define (helper left-over result)
      (if (empty? left-over)
          result
          (helper (cdr left-over) (append result (list (get-left-and-right-padding x (car left-over)))))
          )
      )
    (append (list (get-top-or-bottom-padding x (+ 2 (length xs)))) (helper xs '()) (list (get-top-or-bottom-padding x (+ 2 (length xs)))))
    )
  )

;((pad '()) 0)

(equal? ((pad'( (1 2 3)
                (4 5 6)
                (7 8 9) )
) 0)
        '( (0 0 0 0 0)
           (0 1 2 3 0)
           (0 4 5 6 0)
           (0 7 8 9 0)
           (0 0 0 0 0) ))

(equal? ((pad '( (1 2 3)
                 (4 5 6)
                 (7 8 9) )
) 9)
        '( (9 9 9 9 9)
           (9 1 2 3 9)
           (9 4 5 6 9)
           (9 7 8 9 9)
           (9 9 9 9 9) ))