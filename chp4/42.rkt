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

; Exercise 4.30
; a) When we hit the begin in the for-each loop, the interpreter is going to do an evaluate sequence
; where an eval is called on the (proc (car items)) - evaluating a procedure application which results in the display.
; b) Procedure definitions have an implicit begin in them resulting in an evaluate sequence. (p1 1) produces (1 . 2)
; (p2 1) produces (1 . thunk) because the set application gets thunkified before calling the inner p procedure. Without
; forcing that thunk when we evaluate the sequence of expressions, it remains a thunk.
; With Cys modification though, that thunk will get forced so (p2 1) will end up being the same as (p1 1).
; c) No change in the for-each loop because (proc (car items)) will have already resulted in a primitive value, so 
; forcing it doesn't do anything 
; d) I think it makes sense to force everything in the sequence (cy's approach) because I can't see why they 
; wouldn't be pure side effects. 


; Exercise 4.33
; Problem is quoted expressions are different than lists produced
; with the new cons. 
; Want to be able to pass a quoted list into the interpreter
; Modify text-of-quotation to produce a new list
; if text is ('quote . (1 2 3))
; then we need to convert (cdr text) into a consed list 
; walk through the non lazy list building a to-be-evaluated 
; version of a lazy list (then youd need to evaluate it in the env)
(define (text-of-quotation text)
    (define (make-lazy-list non-lazy-list) 
        (if (null? non-lazy-list) 
            (list 'quote '())
            (list 'cons (list 'quote (car non-lazy-list)) (make-lazy-list (cdr non-lazy-list))))
    )
    (if (pair? text) (make-lazy-list (cadr text)) text)
)
