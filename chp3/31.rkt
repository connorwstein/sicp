#lang sicp

; Bad because other procedures can mess with this
; (define balance 100)
; 
; ; note the special syntax for updating a variable
; (define (withdraw amount)
;   (if (>= balance amount)
;       (begin (set! balance (- balance amount)) ; 
;              balance)
;       "Insufficient funds"))
; 
; Better, but still hard to tell why this procedure behaves the way it 
; does as an external user
; (define new-withdraw
;   (let ((balance 100))
;     (lambda (amount)
;       (if (>= balance amount)
;           (begin (set! balance (- balance amount))
;                  balance)
;           "Insufficient funds"))))


; Exercise 3.1
(define (make-accumulator initial) 
	(let ((sum initial))
		(lambda (amount)
			(begin (set! sum (+ sum amount)) sum)
		)
	)	
)
(define A (make-accumulator 5))
(A 10)
(A 10)


; Exercise 3.2
; reset-counter and how-many-calls are special
(define (make-monitored f) 
	(define count 0)
	(define (how-many-calls) 
		count)
	(define (reset-counter)
		(set! count 0))
	
	(lambda (input) 
		(cond ((eq? input 'reset-counter) (reset-counter))
			  ((eq? input 'how-many-calls) count)
			  (else (begin  (set! count (+ count 1)) (f input)))))
)
(define (square x) (* x x))
(define mf (make-monitored square))
(mf 2)
(mf 'how-many-calls)
(mf 3)
(mf 'how-many-calls)
(mf 'reset-counter)
(mf 'how-many-calls)


; Exercise 3.5
; Estimating integrals with a monte-carlo algorithm
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(display "Monte carlo" )
(newline)
(random-in-range 0 100)
; Estimate the region size of P
(define (estimate-integral P x1 y1 x2 y2) 
	(define (evaluate)
		(P (random-in-range x1 x2) (random-in-range y1 y2)))
	(* 4.0 (monte-carlo 10000 evaluate))
)
; Return whether point is in the unit circle
; x^2 + y ^2 < 1
(define (P x y)
	(<= (+ (square x) (square y)) 1)
)

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(estimate-integral P -1.0 -1.0 1.0 1.0)




