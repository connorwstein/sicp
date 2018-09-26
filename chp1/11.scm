; To run mit-scheme < chapter1.scm

; Expressions
(+ 2 3)
; Compound expressions (functions)
(define (square x) (* x x))
(square 2)
(define (sumofsquares x y)
    (+ (square x) (square y)))
(sumofsquares 2 1)
; Cond --> conditionals
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
(abs -20)
; Can also use this (if <predicate> <consequent> <alternative>)
; which is like a ternary operator in C
; Logical operators:
(define x 2)
(and (< x 5) (> x 10)) ; will evaluate to #f meaning false

; Exercise 1.1
10  ; evaluates to 10
(+ 5 3 4) ; evaluates to 12
(- 9 1) ; 8
(/ 6 2) ; 3
(+ (* 2 4) (- 4 6)) ; 8 + -2 = 6
(define a 3) ; a = 3
(define b (+ a 1)) ; b = 4
(+ a b (* a b)) ; 3 + 4 + 12 = 19
(= a b) ; #f
(if (and (> b a) (< b (* a b)))
      b
	  a) ;if 4 > 3 and 4 < 12 --> 4
(cond ((= a 4) 6)
            ((= b 4) (+ 6 7 a))
                  (else 25)) ; a != 4, b==4 so return 13+3 = 16
(+ 2 (if (> b a) b a)) ; + 2 4 = 6
(* (cond ((> a b) a)
                  ((< a b) b)
                           (else -1))
      (+ a 1)) ; 4 * 4 = 16

; Exercise 1.3
; Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers
; x > z and y > z
; y > x and z > x
; x > y and z > y
(define (>= x y)
    (or (> x y) (= x y)))
(define (twolargesumsquares x y z)
    (cond ((and (>= x z) (>= y z)) (+ (square x) (square y)))
          ((and (>= y x) (>= z x)) (+ (square y) (square z)))
          ((and (>= x y) (>= z y)) (+ (square x) (square z)))))
(twolargesumsquares 1 2 3)

; sqrt program
(define (average x y)
	(/ (+ x y) 2))
;(define (good-enough? y x)
;	(< (abs (- (* y y) x)) 0.001))
 (define (good-enough? guess prev-guess)
   (< (abs (- guess prev-guess)) (abs (* guess 0.001))))
;(define (improve guess x)
;	(average guess (/ x guess)))
; Function taking in a guess and x (two variables)
; Uses the if predicate true false structure
; Recursively calls sqrt-iter with an improved guess and the same x
(define (sqrt-iter guess x)
	(if (good-enough? guess x)
		guess
		(sqrt-iter (improve guess x)
					x)))

(define (sqrt x)
	(sqrt-iter 1.0 x))

;(sqrt 9.0)

; Exercise 1.6
; What happens to sqrt if new-if below is used instead
;(define (new-if predicate then-clause else-clause)
;	(cond (predicate then-clause)
;		(else else-clause)))
; You will get maximum recursion depth because of the applicative order execution all arguments are evaluated before
; executing a procedure (procedure is like a function, defined using the "define" keyword)

; Exercise 1.8
;(define (improve-cube-guess y x)
;;	(/ (+ (/ x (* y y))
;	   	   (* 2.0 y))
;    3.0))
;(define (improve-cube-guess y x)
; (/	(+ (/ x (* y y)) (* 2 y)) 3))
;
;(define (cube-root-iter guess prev-guess x)
;	(begin (display guess) (display prev-guess) (newline)
;	(if (good-enough? guess prev-guess)
;		guess
;		(cube-root-iter (improve-cube-guess guess x) guess x))))
;;(trace cube-root-iter)
;(define (cube-root x) (cube-root-iter (improve-cube-guess 1.0 x) 1 x))
 (define (cube-roots-iter guess prev-guess input)
   (if (good-enough? guess prev-guess)
       guess
       (cube-roots-iter (improve guess input) guess input)))

; (define (good-enough? guess prev-guess input)
;   (> 0.001 (/ (abs (- guess prev-guess))
;               input))) ;; this should be (abs input) to handle negative inputs. Example: (cube-roots -1) should be -1. Before change, output was 0.33. After fix, output is corrected to -1.000000001794607.

 (define (improve guess input)
   (/ (+ (/ input (square guess))
      (* 2 guess))
    3))

 (define (square x)
   (* x x))

 ;;to make sure the first input of guess and prev-guess does not pass the predicate accidentally, use improve here once:
 ;;to make sure float number is implemented, use 1.0 instead of 1:
 (define (cube-roots x)
   (cube-roots-iter (improve 1.0 x) 1 x))

(cube-roots 27)



