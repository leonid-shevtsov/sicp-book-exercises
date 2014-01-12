#lang racket
(define (pascal level i)
  (define (dec i) (- i 1))
  (define (inc i) (+ i 1))
  (cond
    ((= i 1) 1)
    ((= i level) 1)
    (else (+ (pascal (dec level) (dec i)) (pascal (dec level) i)))
    )
  )

(pascal 5 1)
(pascal 5 2)
(pascal 5 3)
(pascal 5 4)
(pascal 5 5)