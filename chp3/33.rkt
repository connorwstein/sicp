#lang sicp
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
; (define a (cons 'a (cons 'b (cons 'c '())))) 
; ; Results in 4
; (define a (list 'test))
; (define b (cons a a))
; (define c (cons b '()))
; ; Results in 7 
; (define a (list 'test))
; (define b (cons a a))
; (define c (cons b b))
; ; Results in infinite loop
; (define z1 (list 'a))
; (define z2 (list 'b))
; (define z3 (list 'c))
; (set-cdr! z1 z2)
; (set-cdr! z2 z3)
; (set-cdr! z3 z3)

; Exercise 3.17 
; count the number of pairs in an arbitrary list structure
; count the number of pairs in the car and cdr of the current pair
; ignore pairs we have already seen and add them to the seen list of pairs
(define (count-pairs-2 x)
  (define seen '())
  (define (present? item item-list)
    (cond ((null? item-list) #f)
          ((eq? item (car item-list)) #t)
          (else (present? item (cdr item-list)))))
  (define (add-to-seen item)
    (set! seen (append seen (list item)))
  )
  (define (iter rem) 
    (cond ((not (pair? rem)) 0)
          ((present? rem seen) 0)
          (else (begin (add-to-seen rem) 
                       (+ (iter (car rem))
                          (iter (cdr rem))
                          1)))))
  (iter x) 
)

; Exercise 3.18
; Check if there is a cycle
; Keep adding nodes to seen if one is already there, we have a cycle
(define (check-cycle x)
  (define seen '())
  (define (iter rem) 
    (cond ((not (pair? rem)) #f)
          ((memq rem seen) #t)
          (else (begin (set! seen (cons rem seen))
                       (or (iter (car rem)) (iter (cdr rem)))))))
  (iter x) 
)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle '(1 2 3)))
(display "Cycle check")
(newline)
(check-cycle z) ; #t 
(check-cycle '(1 2 3)) ; #f


; Exercise 3.19
; Use the fast slow pointer trick
(define (check-cycle-fast-slow x)
     (define (safe-cdr l) 
         (if (pair? l) 
             (cdr l) 
             '())) 
    (define (helper p1 p2)
        (cond ((eq? p1 p2) #t)
              ((or (not (pair? p2)) (not (pair? p1))) #f)
              (else (helper (safe-cdr p1) (safe-cdr (safe-cdr p2))))))
    (if (not (pair? x)) #f (helper x (safe-cdr (safe-cdr x))))
)
(check-cycle-fast-slow z)
(check-cycle-fast-slow '(1 2 3)) ; #f





