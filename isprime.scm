#lang racket

(define (even? n) (= 0 (remainder n 2)))

(define (smallest-divisor n)
  (define (iter i n)
    (define (next n) (if (= n 2) 3 (+ n 2)))
    (cond
      ((> (* i i) n) n)
      ((= 0 (remainder n i)) i)
      (else (iter (next i) n))
      )
    )
  (iter 2 n)
  )

(define (prime? n)
  (= n (smallest-divisor n))
  )

(define (fermat-prime? n)
  (define (expmod base exp m)
    (define (square x) (* x x))
    (cond ((= exp 0) 1)
          ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
          (else (remainder (* base (expmod base (- exp 1) m)) m))
          )
    )
  
  (define (try-it a) (= (expmod a n n) a))
  
  (define (fermat-test n) (try-it (+ 1 (random (- n 1)))))
  
  (define (fermat-tests n times) 
    (cond 
      ((= times 0) true)
      ((fermat-test n) (fermat-tests n (- times 1)))
      (else false)
      )
    )
  
  (fermat-tests n 10)
  )

(define (timed-prime-test method n)
  (define (start-prime-test n start-time)
    (define (report-prime elapsed-time)
      (display " *** ")
      (display elapsed-time)
      )
    (if (method n)
        (report-prime (- (current-milliseconds) start-time))
        false
        )
    )
  (newline)
  (display n)
  (start-prime-test n  (current-milliseconds))
  )

(define (search-for-primes method from to)
  (cond
    ((even? from) (search-for-primes method (+ from 1) to))
    ((< from to) 
     (timed-prime-test method from)
     (search-for-primes method (+ from 2) to)
     )
   )
  )

(search-for-primes prime? 4000000000 4000000100)

(search-for-primes fermat-prime? 4000000000 4000000100)
