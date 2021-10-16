#lang racket

(define (rev n)
  (define (helper result left-over)
    (if (= left-over 0)
        result
        (helper (+ (* 10 result) (remainder left-over 10)) (quotient left-over 10))
        )
    )
  (if (< n 0)
      (error "n was negative")
      (helper 0 n)
      )
  )

(define (palindrome? n)
  (= n (rev n))
  )

(define (num-palindromes-rec a b)
  (if (> a b)
      (num-palindromes-rec b a)
      (cond
        [(= a b) (if (palindrome? a)
                     1
                     0
                     )]
        [(palindrome? a) (add1 (num-palindromes-rec (add1 a) b))]
        [else (num-palindromes-rec (add1 a) b)]
        )
      )
  )

(define (num-palindromes-iter a b)
  (define (helper counter current-number upper-limit)
    (cond
      [(> current-number upper-limit) counter]
      [(palindrome? current-number) (helper (add1 counter) (add1 current-number) upper-limit)]
      [else (helper counter (add1 current-number) upper-limit)]
      )
    )

  (if (> a b)
      (helper 0 b a)
      (helper 0 a b)
      )
  )

(= (num-palindromes-rec 1 101) 19)
(= (num-palindromes-rec 1 100) 18)
(= (num-palindromes-rec 100 1) 18)

(= (num-palindromes-iter 1 101) 19)
(= (num-palindromes-iter 1 100) 18)
(= (num-palindromes-iter 100 1) 18)