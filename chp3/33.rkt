; Exercise 3.12 
; (define x (list 'a 'b))
; (define y (list 'c 'd))
; (define z (append x y))
; z
; (a b c d)
; (cdr x)
; <response> --> (b) 
; (define w (append! x y))
; w
; (a b c d)
; (cdr x)
; <response> (b c d)


; Exercise 3.13
; its a loop, so last-pair will be an infinite loop

; Exercise 3.14
; mystery is reversing the list i.e. converting a->b->c->d to d->c->b->a

; Exercise 3.15
; Both car and cdr of z1 point to the same box of pointers, but the car and cdr of z2 point to different boxes of pointers. 
; This means that set-to-wow will change the only box of pointers in z1 and will change just one of the two boxes in z2 (one of them
; remaining to point to a).  

; Exercise 3.16
; Results in 3
(define a (cons 'a (cons 'b (cons 'c '())))) 
; Results in 4
(define a (list 'test))
(define b (cons a a))
(define c (cons b '()))
; Results in 7 
(define a (list 'test))
(define b (cons a a))
(define c (cons b b))
; Results in infinite loop
(define z1 (list 'a))
(define z2 (list 'b))
(define z3 (list 'c))
(set-cdr! z1 z2)
(set-cdr! z2 z3)
(set-cdr! z3 z3)
