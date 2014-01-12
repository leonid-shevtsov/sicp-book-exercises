#lang racket
(define (fib3_recursive n)
  (if (< n 3)
      n
      (+ (fib3_recursive (- n 1))
         (* 2 (fib3_recursive (- n 2)))
         (* 3 (fib3_recursive (- n 3)))
         )
      )
  )

(define (fib3_iterative n)
  (define (fib3-iter n1 n2 n3 i n)
    (if (= i n)
        n1
        (fib3-iter n2 n3 (+ n3 (* 2 n2) (* 3 n1)) (+ i 1) n)
        )
    )
  (fib3-iter 0 1 2 0 n)
  )
      

(fib3_recursive 10)
(fib3_iterative 10)