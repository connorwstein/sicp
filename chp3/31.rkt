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












