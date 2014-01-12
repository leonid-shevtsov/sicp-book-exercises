#lang racket

(define (even? n) (= 0 (remainder n 2)))
(define (double a) (* a 2))
(define (halve a) (/ a 2))


(define (fast-expt b n)
  (define (fast-expt-iter a b n)
    (if (= n 0)
        a
        (if (even? n)
            (fast-expt-iter a (* b b) (/ n 2))
            (fast-expt-iter (* a b) b (- n 1))
            )
        )
     )
  (fast-expt-iter 1 b n)
  )

(define (mul a b)
  (if (= b 0)
      0
      (if (even? b)
          (mul (double a) (halve b))
          (+ a (mul a (- b 1)))
          )
      )
  )

(define (mul-iterative a b)
  (define (mul-iter a b c)
    (if (= b 0)
        c
        (if (even? b)
            (mul-iter (double a) (halve b) c)
            (mul-iter a (- b 1) (+ c a))
            )
        )
    )
  
  (mul-iter a b 0)
  )

; p = pp + qq
; q = 2pq + qq

(define (fib-log n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          (
           (even? count) 
           (fib-iter a b (+ (* p p) (* q q)) (+ (* 2 p q) (* q q)) (/ count 2))
           )
          (
           else 
           (fib-iter (+ (* b q) (* a q) (* a p)) (+ (* b p) (* a q)) p q (- count 1))
           )
          )
    )
  (fib-iter 1 0 0 1 n)
  )
           
(fast-expt 2 3)

(mul 3 10)

(mul-iterative 3 20)

(fib-log 8) ; 21

