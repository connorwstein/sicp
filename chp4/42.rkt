; Exercise 4.25
; (define (factorial n)
;   (unless (= n 1)
;           (* n (factorial (- n 1)))
;           1))
; In an applicative-order language this will NOT work because the (* n (factorial (- n 1))) gets evaluated
; every time regardless of (= n 1) so it will enter an infinite loop 


; Exercise 4.27
; (define count 0)
; (define (id x)
;   (set! count (+ count 1))
;   x)
; (define w (id (id 10)))
; ;;; L-Eval input:
; count
; ;;; L-Eval value:
; 1   
; ;;; L-Eval input:
; w
; ;;; L-Eval value:
; 10
; ;;; L-Eval input:
; count
; ;;; L-Eval value:
; 2 

; Why the 1?
; The outer "id" call creates a thunk to represent its (id 10) argument. 
; This thunk is (list 'thunk (id 10) global-env)
; When eval-definition (eval (definition-value exp) env) is called it doesn't actually
; do any forcing of the inner thunk because its just a normal eval not an "actual-value". 
; but the eval-definition calls apply which hits the compound-procedure? clause:
;  (eval-sequence
;   (procedure-body procedure) --> This evaluates the body of id 
;   (extend-environment
;    (procedure-parameters procedure)
;    (list-of-delayed-args arguments env) ; changed
;    (procedure-environment procedure))))
