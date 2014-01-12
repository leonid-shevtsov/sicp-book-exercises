#lang racket

(define (even? n) (= 0 (remainder n 2)))

(define (miller-rabin-prime? n)
  (define (expmod2 base exp m)
    (define (square x) (* x x))
    (cond ((= exp 0) 1)
          ((even? exp) (remainder (square (expmod2 base (/ exp 2) m)) m))
          (else (remainder (* base (expmod2 base (- exp 1) m)) m))
          )
    )
  
  (define (try-it a) (= (expmod2 a (- n 1) n) a))
  
  (define (miller-rabin-test n) (try-it (+ 1 (random (- n 1)))))
  
  (define (miller-rabin-tests n times) 
    (cond 
      ((= times 0) true)
      ((miller-rabin-test n) (miller-rabin-tests n (- times 1)))
      (else false)
      )
    )
  
  (miller-rabin-tests n 10)
  )

(miller-rabin-prime? 6601)