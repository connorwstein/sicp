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


; Exercise 2.56 
; show how to extend the basic differentiator to handle more kinds of expressions
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

;exponentiation?, base, exponent, and make-exponentiation
; ** 2 3 --> 2 to the power of 3
(define (exponentiation? x)
    (and (pair? x) (eq? (car x) '**))
)
(define (base x) (cadr x)) 
(define (exponent x) (caddr x)) 
; anything to raised to the power of 0 is 1 and anything raised to the power 1 is itself
(define (make-exponentiation b e)
    (cond ((= e 0) 1)
          ((= e 1) b)
          ((and (number? b) (number? e)) (exp b e))
          (else (list '** b e)))
) 

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp) 
            (display "exp")
            (make-product (exponent exp) 
                          (make-product (make-exponentiation (base exp) (make-sum (exponent exp) -1)) 
                                        (deriv (base exp) var)))
        )
        (else
         (error "unknown expression type -- DERIV" exp))))
(display "taking derivative of x + 3 w.r.t x" )
(newline)
(deriv '(+ x 3) 'x)
(display "taking derivative of 3*x w.r.t x" )
(newline)
(deriv '(* x 3) 'x)
(display "taking derivative of x^3 w.r.t x" )
(newline)
(deriv '(** x 3) 'x)
; Exercise 2.60
(define (element-of-set? s e)
  (define (iter curr)
    (if (null? curr) 
         #f 
         (if (= (car curr) e) #t (iter (cdr curr))))
  )
  (iter s)
)
(element-of-set? (list 1 2 3) 4)

(define (adjoin-set s e)
  (cons e s)
)
(adjoin-set (list 1 2 3) 4)

(define (union-set s1 s2)
  (append s1 s2)
)
(union-set (list 1 2 3) (list 4 5 6))

; Exercise 2.63
; Do the following procedures produce the same two lists 
; for every tree
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

; If the tree is empty return an empty list
; otherwise create a list of left-subtree + current + right-subtree
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

; Builds up a list working on the right subtree first
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))



