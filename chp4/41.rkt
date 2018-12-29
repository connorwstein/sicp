; Exercise 4.1 
; Current implementation of list-of-values may evaluate from left to right or right to left
; depending on how cons in the underlying lisp works
; (define (list-of-values exps env)
;   (if (no-operands? exps)
;       '()
;       (cons (eval (first-operand exps) env)
;             (list-of-values (rest-operands exps) env))))

; Forced left to right
; (define (list-of-values exps env)
;   (if (no-operands? exps)
;       '()
; 		(let (first (eval (first-operand exps) env))
; 			 (rest  (list-of-values (rest-operands exps) env))
; 		(cons first rest))))
; right to left - same thing but do rest first 


; Exercise 4.2 
; What if we reorder this so application? is before the assignment?
; (define (eval exp env)
;   (cond ((self-evaluating? exp) exp)
;         ((variable? exp) (lookup-variable-value exp env))
;         ((quoted? exp) (text-of-quotation exp))
;         ((assignment? exp) (eval-assignment exp env))
;         ((definition? exp) (eval-definition exp env))
;         ((if? exp) (eval-if exp env))
;         ((lambda? exp)
;          (make-procedure (lambda-parameters exp)
;                          (lambda-body exp)
;                          env))
;         ((begin? exp) 
;          (eval-sequence (begin-actions exp) env))
;         ((cond? exp) (eval (cond->if exp) env))
;         ((application? exp)
;          (apply (eval (operator exp) env)
;                 (list-of-values (operands exp) env)))
;         (else
;          (error "Unknown expression type -- EVAL" exp))))
; a) The problem is that the logic is to handle everything that is explicitly not a procedure application first
; and whatever remains is a procedure application. If you feed it (define x 3) then its going to think 
; thats an application of "define" to args x, 3 (not recognizing that x is a symbol). 
; b) 
; (define (application? exp) (tagged-list? exp 'call))
; (define (operator exp) (cadr exp))
; (define (operands exp) (cddr exp))

; Exercise 4.3
; Rewrite eval so its done in a data directed style, that would mean there a table formed via:
; "put op type item" and "get op type".
; In the case of eval, the op is always to evaluate but the type might be variable, quoted etc. 
; We can assume the car of exp is the type.  
; ex:
; (put 'eval 'quoted lookup-variable-value)
; (put 'eval 'assignment eval-assignment)

; (define (eval exp env)
;   (cond ((self-evaluating? exp) exp)
;         ((variable? exp) (lookup-variable-value exp env))
; 		  ((get 'eval (car exp)) ((get 'eval (car exp)) exp env)) ; recursively evaluate compound expressions (base case is self-evaluating or variable
; 		  ((application? exp) 
; 			(apply (eval (operator exp) env) (list-of-values (operands exp) env)))
;         (else
;          (error "Unknown expression type -- EVAL" exp))))
; ) 


; Exercise 4.6
; Implement a syntactic transformation let->combination that reduces evaluating let expressions to evaluating combinations of the type shown above, and add the appropriate clause to eval to handle let expressions.
; (let ((<var1> <exp1>) ... (<varn> <expn>))
;   <body>)
; 
; is equivalent to
; 
; ((lambda (<var1> ... <varn>)
;    <body>)
;  <exp1>
;  
;  <expn>)
; let-statement is a list of (let (list of pairs) body)
; Need to extract 
; cadr let-statemnet --> list of pairs
; need to get the first element of each and build a list 
; cddr let-statemnet --> body
(define (let->combination let-statement) 
	(define (get-vars pairs) 
		(if ((null? pairs) '())
			(cons (caar pairs) (get-vars (cdr pairs))))
	)
	(define (get-exp pairs)
		(if ((null? pairs) '())
			(cons (cdar pairs) (get-vars (cdr pairs))))
	)
	(cons (make-lambda (get-vars (cadr let-statement)) cddr let-statement) (get-exp (cadr let-statement)))
)


; Exercise 4.11
; Represent the frame as a single list of bindings
; (single list of pairs)
; List of lists
; (define (make-frame variables values)
;     (if (null? (cdr variables)) 
;         (list (cons (car variables) (car values))) 
;         (append (cons (car variables) (car values)) 
;                 (make-frame (cdr variables) (cdr values)))))
; Or list of pairs
(define (make-frame variables values)
    (map cons variables values)
)
(define (frame-variables frame) (map car frame)
(define (frame-values frame) (map cdr frame))


; Exercise 4.14
; Fails because for example (map + '(1 2)) will cause + to be interpreted as a direct application of + 

; Exercise 4.15
; (define (run-forever) (run-forever))
; 
; (define (try p)
;   (if (halts? p p)
;       (run-forever)
;       'halted))
; (halts? p a) returns #t if the application of p with object a halts and #f if it doesn't
; What would happen if we were to evaluate the expression (try try)? 
; (try try)  --> (if (halts? try try) (run-forever) 'halted) 
;            --> if (halts? try try) is true then we call (run-forever) but that would mean try try (from the first call) doesn't actually halt
;            --> if (halts? try try) is false then we return 'halted but that would mean try try (from the first call) does actually halt
;            --> either way there is a contradiction, so p and a cannot be arbitrary since we showed a counter example which doesn't work


; Exercise 4.16
; a) was to add the *unassigned* handling
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            ((eq? (car vals) '*unassigned*) (error "Value is *unassigned*"))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

; b) Scan out defines which takes a procedure body and returns an equivalent one 
; body is a list of expressions 
(define (scan-out body) 
    (define (is-def x) 
        (and (pair? x) (eq? (car x) 'define)))
    (define defs (filter is-def body)
    (define non-defs  (filter (lambda (x) (not (is-def x))) body))
    ; Now build the transformed body one big let expression
    (define unassigned (map (lambda (x) (cons (car x) '*unassigned*)) defs))
    (define sets (map (lambda (x) (list 'set! (car x) (cadr x))) defs))
    ; Wishful thinking - assume we have this variadic function
    ; first argument is a list of pairs for the let and the rest are expressions 
    (make-let unassigned sets non-defs) 
)

; Exercise 4.20 a)
; letrec is a variation on let where the expressions which provide initial values for the variables
; are evaluated in an environment which contains all the expressions
; Implement letrec by transforming it into an equivalent let expression we need
; letrec expr is of the form (list 'letrec <list of bindings pairs> <expressions>) 
; Need to convert it to the let unassigned followed by sets (similar to 4.16)
; Need to use a begin so we can have the sets! in there 
(define (unassigned letrec-expr) (map (lambda (x) (cons (car x) '*unassigned*)) (cadr letrec-expr))
(define (sets letrec-expr) (map (lambda (x) (list ('set! (car x) (cadr x))) (cadr letrec-expr))))
(define (letrec->let letrec-expr)
   (list 'let (unassigned letrec-expr) (make-begin (append sets (cddr letrec-expr)))) 
)













