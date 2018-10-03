#lang racket

; (define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline)
)

; (print-rat (make-rat 10 2))
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; Euclids algorithm
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(provide gcd)
(provide make-rat)



; Exercise 2.1 define a better version of make rat that handles both positive and negative numbers 
; Right now the bug is that the gcd may return negative or not depending on the number of iterations involved
; We can just explicitly handle signs 
(define (make-rat-with-sign n b) 
	(define (sign x) (if (< x 0) 1 0))
	(define (make-rat-helper n d neg)
	  (let ((g (gcd n d)))
		(if neg (cons (/ (* -1 n) g) (/ d g)) (cons (/ n g) (/ d g)))))
	(make-rat-helper (abs n) (abs b) (not (= (sign n) (sign b))))
)
(provide make-rat-with-sign)
(make-rat-with-sign 12 -6)
(make-rat-with-sign 12 -8)


; Exercise 2.2 
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment line) (car line))
(define (end-segment line) (cdr line))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline)
)

(define (midpoint line) 
	(define (avg x y) (/ (+ x y) 2.0))
	(make-point (avg (x-point (start-segment line)) (x-point (end-segment line)))
				(avg (y-point (start-segment line)) (y-point (end-segment line))))
)
(define p1 (make-point 0 0))
(print-point p1)
(define p2 (make-point 1 5 ))
(print-point p2)
(define mid (midpoint (make-segment p1 p2)))
(print-point mid)

(define p3 (make-point -1 7))
(print-point p3)
(define p4 (make-point -20 5))
(print-point p4)
(define mid2 (midpoint (make-segment p3 p4)))
(print-point mid2)


; Exercise 2.3
; Representation for rectangles where perimeter and area are independent of the data representation
; Representations could be 4 points, 4 line segments, 2 line segments, 3 line segments, 3 points etc.
; Assume the 4 points/lines are a valid rectangle
; Trick would be have a selector function which gives you the length of side a and side b
; regardless of representation
; points could be out of order
; assumes p1 -- p2 and p2 -- p3  are the line segments
(define (square x) (* x x))
(define (rect-via-points p1 p2 p3) (cons (make-segment p1 p2) (make-segment p2 p3)))
(define (len line) (sqrt (+ (square (- (x-point (start-segment line)) (x-point (end-segment line)))) (square (- (y-point (start-segment line)) (y-point (end-segment line)))))))
(define (side-a-length rect) (len (car rect)))
(define (side-b-length rect) (len (cdr rect)))
(define (perim rect) (+ (* 2 (side-a-length rect)) (* 2 (side-b-length rect))))
(define (area rect) (* (side-a-length rect) (side-b-length rect)))

(define myrect (rect-via-points (make-point 0 10) (make-point 0 0) (make-point 5 0)))
(side-a-length myrect)
(side-b-length myrect)
(area myrect)
(perim myrect)
; Another representation would just need to satisfy the side-a-length side-b-length functions 
; (define (rect-via-lines l1 l2) (cons l1 l2))


; Exercise 2.4
; returns an anonymous function with one argument
(define (mycons x y)
  (lambda (m) (m x y)))

(define (mycar z)
  (z (lambda (p q) p)))
; z is lambda (m) (m x y)
; where m is lambda (p q) p
; the argument to z is a function which operates on x y in the case of car it returns the first of the
; two values - in this case x 
(define (mycdr z)
  (z (lambda (p q) q)))

(define test (mycons 10 11))
(mycar test)
(mycdr test)

; Exercise 2.5 
; show that we can represent pairs of non-negative integers 

















