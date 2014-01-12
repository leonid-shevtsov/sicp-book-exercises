#lang racket

(define (carmichael-number? n) 
  (define (even? n) (= 0 (remainder n 2)))
  (define (expmod base exp m)
    (define (square x) (* x x))
    (cond ((= exp 0) 1)
          ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
          (else (remainder (* base (expmod base (- exp 1) m)) m))
          )
    )
  
  (define (iter a n)
    (cond
      ((= a n) true)
      ((= (expmod a n n) a) (iter (+ a 1) n))
      (else false)
      )
    )
  (iter 1 n)
  )


(carmichael-number? 6601)