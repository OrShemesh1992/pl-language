#lang pl
(: f : Number -> Number)

(define (f x)
  (* x (+ x 1)))
(test (f 0) => 0)

(sqrt (+ (* 3 3) (* 4 4) )); The answer is: 5

(: f1 : Number -> Number)
(define (f1 x)
  (cond [(> x 1) 1]
  [(> x 0) 0]
  [else 2 ]))

(test (f1 0) => 2)
(test (f1 1) => 0)