#lang racket

(define mylist (list 1 2 3 4)) ; actually just a linkedlist of conses

; Note doesn't handle errors
(define (item-at-index l n) 
    (define (iter l i)
        (if (= i n) (car l) (iter (cdr l) (+ i 1)))
    )
    (iter l 0)
)

(item-at-index mylist 2)

; Appending two lists
(define l1 (list 1 2 3))
(define l2 (list 4 5 6))
; Jesus this is beautiful
(define (append-list l1 l2) 
    (if (null? l1) l2 (cons (car l1) (append-list (cdr l1) l2))) 
)
(append-list l1 l2)

; Exercise 2.17 
(define (my-last-pair l1)
    (define (helper prev l)
        (if (null? l) prev (helper (car l) (cdr l)))
    )
    (helper null l1)
)
(my-last-pair (list 1 2 3 4 5 6))

; Exercise 2.20
; . notation means a unlimited number of arguments will be passed in as a list named w
(define (g . w) (car w))
(g 1 2 3 4)
; return a list of arguments that have the same even-odd parity as the first argument
; build the result list as we iterate
; Have a helper which gets called first with the results being the first element in args
; then we iterate through the rest of args adding the element to results if they have the same parity
(define (same-parity first . args) 
    (define (helper res curr-args) 
                (cond ((null? curr-args) res) 
                (else (if (or (and (even? (car curr-args)) (even? first)) 
                              (and (odd? (car curr-args)) (odd? first)))
                       (helper (cons (car curr-args) res) (cdr curr-args))  
                       (helper res (cdr curr-args))))))
    (helper (list first) (cdr args))
)

(same-parity 1 2 3 4 5 6 7) ; will the parity numbers in reverse
(same-parity 2 3 4 5 6 7)

; Maps
; scaling a list up by some value
(define (scale-up x l) 
    (define (iter res remaining)
        (if (null? remaining) res (iter (cons (* x (car remaining)) res) (cdr remaining)))
    )
    (iter null l) 
)
(scale-up 2 (list 1 2 3 4)) ; scaled in reverse
; Not in reverse
(define (scale-up2 x l)
    (if (null? l) null (cons (* x (car l)) (scale-up2 x (cdr l)))))
(scale-up2 2 (list 1 2 3 4))


















