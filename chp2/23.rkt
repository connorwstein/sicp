#lang racket

; Use the ' as a literal quotation
(define t '(a b c))
(car t)

;Memq returns the sublist starting at the search pattern
(memq 'apple '(dog apple pear)) ; i.e. '(apple pear)

; Exercise 2.54
(define (my-eq x1 x2) 
   (cond ((and (not (pair? x1)) (not (pair? x2))) 
          (eq? x1 x2)) 
         ((and (pair? x1) (pair? x2)) 
          (and (my-eq (car x1) (car x2)) (my-eq (cdr x1) (cdr x2)))) 
         (else false))) 
  

(my-eq (list 1 2) (list 1 3))
(my-eq '(a b) '(a b))
(my-eq '(a b) '((1 2) b))
(my-eq '((1 2) b) '((1 2) b))




