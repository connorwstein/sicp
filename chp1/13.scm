#lang racket

; 1.3 Higher order procedures
; Generic pattern for summation
(define (sum term a next b)
   ;(begin (display (term a)) (newline))
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (cube a) (* a a a))
(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 0 10)
(define (for-loop low high inc) 
  (cond ((not (= low high)) (display low) (for-loop (+ low inc) high inc)))
)
(for-loop 0 10 1)

; Exercise 1.29 Simpsons rule
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
(integral cube 0 1 0.01)

(define (even? a)
  (= (remainder a 2) 0))

; Get the n'th simpson number 
; if even 2* f(n), if odd its 4*f(n)
(define (simpsons f a b n) 
    (define h (/ (- b a) n))
    ; k is the interval number
    (define (yk k) (f (+ a (* k h))))
    (define (term curr) 
        (cond ((= curr 0) (yk curr))
              ((= curr n) (yk curr))
              (else (if (even? curr) (* 2 (yk curr)) (* 4 (yk curr))))
    ))
    (* (/ h 3) 
       (sum term 0 inc n)
    )
)

(simpsons cube 0 1 100)
(integral cube 0 1 0.01)

;Exercise 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (display result)
    (if (> a b)
        result 
        (iter (next a) (+ result (term a)))))
  (iter a 0)
)

(sum-iter cube 0 inc 3) ; 1 + 2^3 + 3^3 = 36
(sum-iter cube 0 inc 4) ; 1 + 2^3 + 3^3  + 4^3

; Let expressions are syntactic sugar
; for lambas where the arguments are expresssions
; x is 2 
(define x 2)
; evalue x * y where x is 3
; and y is 4
   (let ((x 3)
      (y (+ x 2)))
  (* x y))



; Exercise 1.34
(define (f g)
(g 2))
; This will error because it will evaluate f 2 
; which the will be 2 2 which doesn't make sense 
;(f f)


; Exercise 1.35 
; simple - phi^2 = phi + 1, divide both sides by phi 
; you get phi = 1 + 1/phi which is the fixed point function


; Exercise 1.37 
; continued fractions
(define (cont-frac n d k) 
    (define (helper curr) 
        (cond ((= curr k) (/ (n curr) (d curr)))
              (else (/ (n curr) 
               (+ (d curr) (helper (+ curr 1)))))))
    (helper 1)
)

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           100)


; Returning procedures
; Example - here we return a function say g where g(x) = avg(x, f(x))
; (define (average x y)
;   (/ (+ x y) 2))
; (define (average-damp f)
;   (lambda (x) (average x (f x))))
; 
; ; Taking the deriviative of a function, returns another function
; (define dx 0.00001)
; (define (deriv g)
;   (lambda (x)
;     (/ (- (g (+ x dx)) (g x))
;        dx)))
; 
; (define tolerance 0.00001)
; (define (fixed-point f first-guess)
;   (define (close-enough? v1 v2)
;     (< (abs (- v1 v2)) tolerance))
;   (define (try guess)
;     (let ((next (f guess)))
;       (if (close-enough? guess next)
;           next
;           (try next))))
;   (try first-guess))
; (define (newton-transform g)
;   (lambda (x)
;     (- x (/ (g x) ((deriv g) x)))))
; (define (newtons-method g guess)
;   (fixed-point (newton-transform g) guess))
; 
; ; Exercise 1.40
; ; Define a procedure cubic s.t. newtons method approximates the zeros 
; ; return a
; (define (cubic a b c) 
;     (lambda (x) (+ (* a (cube x)) (* b (* x x)) c)))
; (newtons-method (cubic 1 1 1) 1)
