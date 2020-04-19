#lang pl

#|
This function get list of string and return boolean if and only if
exists suffix "pl" in one of the strings 
I used these functions

I searched for data on the language on the site:
https://docs.racket-lang.org/reference/strings.html

Example function use:

(substring "Apple" 1 3)
return "pp"
(string-length "Apple")
return 5
(rest '(1 2 3 4 5 6 7 8 9 10))
return '(2 3 4 5 6 7 8 9 10)
(first '(1 2 3 4 5 6 7 8 9 10))
return 1

In this function, the difficulty was to
understand the syntax of the language pl.
|#


(: plSuffixContained : (Listof String) -> Boolean)

(define(plSuffixContained lists)
  (cond [(null? lists) #f]
        [(<= (string-length (first lists)) 1) (plSuffixContained(rest lists))]
        [(equal? (substring (first lists) (- (string-length (first lists)) 2)) "pl") #t]
        [else (plSuffixContained(rest lists))]))

#|
I added tests to the function to check an extreme case.
|#

(test (plSuffixContained '("pllp" "plyy" "ppp" "lpTT" "lol")) => false)
(test (plSuffixContained '()) => false)
(test (plSuffixContained '("pl")) => true)
(test (plSuffixContained '("plllpl" "plyy" "ppp" "lpTT" "lol")) => true)
(test (plSuffixContained '("pllp" "pllyypl" "ppp" "lpTT" "lol")) => true)
(test (plSuffixContained '("pllp" "plyy" "ppp" "lpTT" "plopl")) => true)
(test (plSuffixContained '("pt" "l" "l" "pl")) => true)



#|

This function get list of number and return string
The polynomial Representing the list.
example: '(3 2) -> "3x+2" 


In addition to the functions in Question 1
I used:

(string-append "Apple" "Banana")
return "AppleBanana"
(string=? "Apple" "Banana")
return #f
(string->number "-7")
 convert to number and return -7

The key issue in question 2 was to check all end cases
that could be in a standard polynomial and
that the user would intuitively accept it.

I used a tail recursion with a shell
function in both question 2.1 and question 2.2

|#

;Main function
(: write-poly : (Listof Number) -> String)
(define (write-poly list)
  (write-poly-help list ""))

;function helper to write-poly for tail recursion
  (: write-poly-help : (Listof Number) String -> String)
  (define ( write-poly-help list str)

  (cond
    [(null? list) str]
 [(and (= (first list) 0) (string=? "" str) (= (length list) 1))
  (write-poly-help (rest list) (string-append str "0"))]
 
    [(= (first list) 0) 
  (write-poly-help (rest list) (string-append str ""))]
    
[(string=? "" str )
 (cond
      [(= (length list) 2)
      (write-poly-help (rest list) (string-append str (number->string (first list)) "x"))]
      [(= (length list) 1)
      (write-poly-help (rest list) (string-append str (number->string (first list))))]
      [else
      (write-poly-help (rest list) (string-append str (number->string (first list)) "x^" 
      (number->string (- (length list) 1))))])]

[(> (first list) 0)
 (cond
[(= (length list) 2)
 (write-poly-help (rest list) (string-append str "+" (number->string (first list)) "x"))]
[(= (length list) 1)
 (write-poly-help (rest list) (string-append str "+" (number->string (first list))))]
[else (write-poly-help (rest list) (string-append str "+" (number->string (first list)) "x^" 
(number->string (- (length list) 1))))])]

[else
  (cond
[(= (length list) 2)
 (write-poly-help (rest list) (string-append str (number->string (first list)) "x"))]
[(= (length list) 1)
 (write-poly-help (rest list) (string-append str (number->string (first list))))]
[else (write-poly-help (rest list) (string-append str (number->string (first list)) "x^" 
(number->string (- (length list) 1))))])]))

#|
I added tests to the function to check an extreme case.
|#

(test (write-poly '(3 2)) => "3x+2")
(test (write-poly '(3)) => "3")
(test (write-poly '(3 2 6)) => "3x^2+2x+6")
(test (write-poly '()) => "")
(test (write-poly '(7 8 9 10)) => "7x^3+8x^2+9x+10")
(test (write-poly '(-3 2 6)) => "-3x^2+2x+6")
(test (write-poly '(3 0 2)) => "3x^2+2")
(test (write-poly '(1 0 1)) => "1x^2+1")
(test (write-poly '(0 0 2)) => "2")
(test (write-poly '(0 1 2)) => "1x+2")
(test (write-poly '(3 -2 6)) => "3x^2-2x+6")
(test (write-poly '(-3 -2 6)) => "-3x^2-2x+6")
(test (write-poly '(8 9 -10 1)) =>"8x^3+9x^2-10x+1")
(test (write-poly '(2 0 0 0 0 6 -3)) =>"2x^6+6x-3")
(test (write-poly '(0 0 0)) => "0")

#|
This function gets a list of numbers and number
And calculates the value of the polynomial in the list.

In this function I used Question 2.1 to
think of cases only in the calculation. 

|#

;Main function
(: compute-poly : Number (Listof Number) -> Number)
(define (compute-poly list x)
  (compute-poly-help x list 0))

;function helper to compute-poly for tail recursion
  (: compute-poly-help : (Listof Number) Number Number -> Number)
  (define (compute-poly-help list x sum)

  (cond [(null? list) sum]

  [(= (length list) 1)
   (compute-poly-help (rest list) x (+ (first list) sum))]

[else (compute-poly-help (rest list) x (+ (* (first list) (expt x (- (length list) 1))) sum))])
)

#|
I added tests to the function to check an extreme case.
|#

(test (compute-poly -2 '(-4 -7)) => 1)
(test (compute-poly -1 '(-4 -7)) => -3)
(test (compute-poly 0 '(-4 -7)) => -7)
(test (compute-poly 1 '(-4 -7)) => -11)
(test (compute-poly 2 '(-4 -7)) => -15)
(test (compute-poly 2 '(-7)) => -7)
(test (compute-poly 2 '(7)) => 7)
(test (compute-poly 2 '()) => 0)
(test (compute-poly -2 '(3 2 6)) => 14)
(test (compute-poly -1 '(3 2 6)) => 7)
(test (compute-poly 0 '(3 2 6)) => 6)
(test (compute-poly 1 '(3 2 6)) => 11)
(test (compute-poly 2 '(3 2 6)) => 22)
(test (compute-poly 3 '(4 3 -2 0)) => 129)


#|
We have defined a new type stack
And created functionality like:
EmptyKS:
Push: get Symbol String Stack


The key issue in Question 3 was to understand how to use
define-type and create funcion for define-type.

In this Question i use:
define-type
Symbol
cases
|#

(define-type Stack
  [EmptyKS]
  [Push Symbol String Stack]
  )

#|
In the function search-stack get Symbol and type Stack
and return or false or string

This function search-stack on stack a specific symbol and return
if is exists else return false.

|#

;search-stack
(: search-stack : Symbol Stack -> (U String #f))
(define (search-stack Sym sta)

( cases sta  
[(Push sy str stack)
 (if (eq? sy Sym)
 str
 (search-stack Sym stack))]
[EmptyKS #f]
  ))

#|
In the function pop stack get Stack
and return stack or false

This function pop-stack return stack if
exists stack we got and false if not exists
|#
;pop stack
(: pop-stack : Stack -> (U Stack #f))
( define ( pop-stack sta)
( cases sta  
[(Push sy str stack) stack]
[EmptyKS #f]
  ))

;tests
(test (EmptyKS) => (EmptyKS))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) =>
(Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))
=> (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a
"A" (EmptyKS))))) => "AAA")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a
"A" (EmptyKS))))) => #f)
(test (search-stack 'a (EmptyKS)) => #f)
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A"
(EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (EmptyKS)) => #f)

#|
In this question you are given full code together with tests for the presented
functions. All you are required to do is to add the appropriate comments for
each of the functions. These comments should describe what the function
takes as input, what it outputs, what its purpose is, and how it operates. Do
not forget to also add your personal remarks on the process in which you
personally came to realize the above. You should copy the following code into
your .rkt file, and add the comment therein.
|#

(: is-odd? : Natural -> Boolean)
;; Signature of the function
;; What the function receives and what it returns
;; the function get number Natural and return true if and only if is odd
(define (is-odd? x)
(if (zero? x)false
(is-even? (- x 1))))
(: is-even? : Natural -> Boolean)
;;  the function get number Natural and return true if and only if is even
;; Users recursively here to search for the number 
(define (is-even? x)
(if (zero? x)
true
(is-odd? (- x 1))))
;; tests --- is-odd?/is-even?
(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))
(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
;; See explanation about the All syntax at the end of the file...
;; This function accepts some variable list and
;; function that can contain the same variable
;; And calls the function from every value in the list
(define (every? pred lst)
(or (null? lst)
(and (pred (first lst))
(every? pred (rest lst)))))
;; An example for the usefulness of this polymorphic function
(: all-even? : (Listof Natural) -> Boolean)
;; the function get list of number Natural
;; and return true if and only if is all the list is even
;; use is even finction helper to all-even.  
(define (all-even? lst)
(every? is-even? lst))
;; tests
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))