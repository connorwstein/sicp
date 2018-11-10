; Exercise 3.9
; Environment structures created by 
;(define (factorial n)
;   (if (= n 1)
;       1
;       (* n (factorial (- n 1)))))
; global env -->  factorial (points to an formal parameter n and a body and a pointer to the global env)
; if we call factorial 3, then its going to create 3 frames all pointing to the global env
; E1 [n = 3] E2 [n = 2] E3 [n = 1]
;
; (define (factorial n)
;   (fact-iter 1 1 n))
; (define (fact-iter product counter max-count)
;   (if (> counter max-count)
;       product
;       (fact-iter (* counter product)
;                  (+ counter 1)
;                  max-count)))
; global env has two procedures defined factorial and fact-iter
; factorial - one formal argument n
; fact-iter - 3 formal arguments
; 
; if we call factorial 3 there is going to be a frame 
; E1 [n=3 and (fact-iter 1 1 3)]
; E2 [product=1 counter=1 max-count=3 and (body of fact-iter with arguments substituted]
; E3 product=2 counter=2 max-count=3
; E4 product=6 counter=3 max-count=3
; E5 product=6 counter=4 max-count=3

; Exercise 3.10
; The difference is that when executing:
; (define (make-withdraw initial-amount)
;   (let ((balance initial-amount))
;     (lambda (amount)
;       (if (>= balance amount)
;           (begin (set! balance (- balance amount))
;                  balance)
;           "Insufficient funds"))))
; (define W1 (make-withdraw 100))
; W1 is globally defined. The enclosing frame of the procedure is first initial-amount=100 and then balance=100
; the !set will mutate the inner enclosing frame (balance=100)

; Exercise 3.11
;(define (make-account balance)
;   (define (withdraw amount)
;     (if (>= balance amount)
;         (begin (set! balance (- balance amount))
;                balance)
;         "Insufficient funds"))
;   (define (deposit amount)
;     (set! balance (+ balance amount))
;     balance)
;   (define (dispatch m)
;     (cond ((eq? m 'withdraw) withdraw)
;           ((eq? m 'deposit) deposit)
;           (else (error "Unknown request -- MAKE-ACCOUNT"
;                        m))))
;   dispatch)
; (define acc (make-account 50))
; The local state for acc is kept in the frame created by the make-account call i.e. with the balance=50 binding
; (define acc2 (make-account 100))
; The states are kept distance because the balance bindings are in different frames. The only shared environment 
; is the global environment. 
