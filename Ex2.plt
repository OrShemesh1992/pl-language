#lang pl
;;list-im
null
'()
(cons 1 (cons 2 null))
(cons 1 (cons 2 '()))
(list 1 2)
'(1 2)
(list? '(1 2))
(list? (cons 1 (cons 2 '())))
(list? 1)
(list 1 2 3 null)
;;pair
(pair? 1)
(pair? (cons 1 (cons 2 '())))
(pair? (list 1 2))
(pair? '());that's null
(pair? '())
#|
get list(ListofAny)
return length (Natural)
if(list-length '()) =0
|#
(: list-length : (Listof Any) -> Natural)
(define(list-length list)
  (if(null? list)
     0
     (+ 1 (list-length(rest list)))))

(test(list-length '(1 2 3 4)) => 4)

;;recourcia of n!
(: fact : Natural -> Natural)
(define (fact n)
  (if(zero? n)
     1
     (* n (fact(- n 1)))))

