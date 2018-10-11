#lang racket

; ---- 2.3.1 -----
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



; ---- 2.3.2 -----
; Exercise 2.56 
; show how to extend the basic differentiator to handle more kinds of expressions

; Exercise 2.57 
; Extend the deriv to support sums and product of abritrary sizes

; Exercise 2.58
; Infix notation
;(x + (3 * (x + (y + 2))))
; Part A is straightforward Need to change our selectors for example
; product? x would just check cadr x for '* rather than car x
; and all the list '* m1 m2 would be come 'm1 * m2
; Part B 
; how to handle for example exp (x + 3 * (x + y + 2))?


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

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
; Augend should be the sum of the rest of the terms
; Trying to be able to support something like (+ x (* 2 x) x y), a sum with three terms
; can we do a fold-left here?
; S could be an expression like 
(define (augend s) 
    (accumulate make-sum 0 (cddr s))
)
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) 
    (accumulate make-product 1 (cddr p))
)

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
(newline)
(deriv '(+ x x (** x 2) x x) 'x)
(newline)
(deriv '(* x x 2) 'x) ; 2x + 2x --> 4x


; ---- 2.3.3 -----
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
; Exercise 2.59 Implement the union set
; elements that are in either set are in the union
; if either set is null return the other set
; otherwise keep 
(define (union-set set1 set2)
  (cond ((or (null? set1) (null? set2)) (if (null? set1) set2 set1))
        ((not (element-of-set? (car set1) set2))
         (cons (car set1)
               (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2)))
)
(print "Union set test")
(newline)
(union-set '(1 2 3) '(3 4))
(newline)
(union-set '(1 2) '(1 2 10 12))


; Exercise 2.60
(define (element-of-set-dup? s e)
  (define (iter curr)
    (if (null? curr) 
         #f 
         (if (= (car curr) e) #t (iter (cdr curr))))
  )
  (iter s)
)
(element-of-set-dup? (list 1 2 3) 4)
(define (adjoin-set-dup s e)
  (cons e s)
)
(adjoin-set-dup (list 1 2 3) 4)
(define (union-set-dup s1 s2)
  (append s1 s2)
)
(union-set-dup (list 1 2 3) (list 4 5 6))

; Exercise 2.61
; adjoin-set using the ordered representation
; Need to find where to insert this guy
; Just walk until you find something greater
(define (adjoin-set-ordered s1 x)
   (define (iter res curr)
    (cond ((null? curr) (append res x))
          (else (if (> (car curr) x) 
                    (append res (cons x curr)) 
                    (iter (append res (list (car curr))) (cdr curr))))))
    (iter null s1) 
)
(adjoin-set-ordered '(1 2 10) 3)

; Exercise 2.62 O(n) implementation of union set
; Can do this like merge sort style?
; if one of the lists is empty, append the other list to the result
; 
(define (union-set-ordered s1 s2)
   (define (iter res c1 c2)
        (cond ((or (null? c1) (null? c2)) (if (null? c1) (append res c2) (append res c1)))
              ((= (car c1) (car c2)) (iter (append res (list (car c1))) (cdr c1) (cdr c2)))
              ((< (car c1) (car c2)) (iter (append res (list (car c1))) (cdr c1) c2))
              ((< (car c2) (car c1)) (iter (append res (list (car c2))) c1 (cdr c2)))
        ))
    (iter null s1 s2)
)
(newline)
(print "union set ordered")
(newline)
(union-set-ordered '(1 2 3) '(3 4 5))



; Exercise 2.63
; Do the following procedures produce the same two lists 
; for every tree
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (element-of-set-tree? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set-tree? x (left-branch set)))
        ((> x (entry set))
         (element-of-set-tree? x (right-branch set)))))

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

(define n4 (list 4 null null)) 
(define n5 (list 5 null null)) 
(define n3 (list 3 n4 n5)) 
(define n2 (list 2 null null)) 
(define n1 (list 1 n2 n3))
; Evaluate to the same list
(tree->list-1 n1)
(tree->list-2 n1)
; However list-1 looks more expensive because of the append


(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))




; ---- 2.3.4 -----
; Huffman encoding



