#lang racket

(define (smallest-divisor n)
  (define (iter i n)
    (cond
      ((> (* i i) n) n)
      ((= 0 (remainder n i)) i)
      (else (iter (+ 1 i) n))
      )
    )
  (iter 2 n)
  )

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)