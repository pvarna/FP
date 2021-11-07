#lang racket

(define (sort-list xs)
  (λ (proc) (sort xs proc))
  )

(define (my-length-compare a b)
  (< (string-length a) (string-length b))
  )

(equal? ((sort-list '("one" "two" "0" "five" "" "one hundred" "onehundred")) my-length-compare) '("" "0" "one" "two" "five" "onehundred" "one hundred"))