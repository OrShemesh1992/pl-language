#lang pl

;;tree abstrect

(define-type AE
  [Num Number]
  [Add AE AE]
  [Sub AE AE])
#|
(string->sexpr "{ + 3 4}") return object sexpr recursive
(string->sexpr "3")
|#
(Add (Sub (Num 3) (Num 4)) (Num 7))
#|
read wite use function sexpr
|#
(: parse : String -> AE)
(define (parse code)
  (parse-sexpr (string->sexpr code)))
#|
parsing to AE read wite use function sexpr
|#
(: parse-sexpr : Sexpr -> AE)
(define (parse-sexpr sexpr)
  (cond [(number? sexpr) (Num sexpr)]
        [(and (list? sexpr) (= (length sexpr) 3) (equal? (first sexpr) '+))
             (Add (parse-sexpr (second sexpr))
                  (parse-sexpr (third sexpr)))]
          [(and (list? sexpr) (= (length sexpr) 3) (equal? (first sexpr) '-))
             (Sub (parse-sexpr (second sexpr))
                  (parse-sexpr (third sexpr)))]
          [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(test (parse "5") => (Num 5))

(test (parse "{+ 6 7}") => (Add (Num 6) (Num 7)))

(test (parse "{+ 5 7 7}") =error> "bad syntax")
