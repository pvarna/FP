#lang racket

(require racket/trace)

(define (concat-proc list1 list2)
  ;(flatten (cons list1 list2)) -> това е глупостта, която написах преди да се сетя, че има append (оставям я за весело)
  (append list1 list2)
  )

(define (concat-rec list1 list2)
  (if (= (length list1) 1)
      (cons (car list1) list2)
      (concat-rec (reverse (cdr (reverse list1))) (cons (last list1) list2))
      )
  )

;(trace concat-rec)
; using a predefined procedure
(equal? (concat-proc '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))

; using a linearly recursive process
(equal? (concat-rec '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))