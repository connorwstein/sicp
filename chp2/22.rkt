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

; Exercise 2.21 
(define (square-list items)
  (if (null? items)
      null
      (cons (* (car items) (car items)) (square-list (cdr items)))))
(define (square-list-2 items)
  (map (lambda (x) (* x x)) items))

(print "sq lists 1")
(square-list (list 1 2 3))
(print "sq lists 2")
(square-list-2 (list 1 2 3))

; Exercise 2.22 its reversed because he isn't consing in the right order
; The second case the list 1 2 3 4 gets built
; via (cons (cons (cons nil 1) 4) 9) which is also not the list we want


; Exercise 2.23
(define (for-each f l)
  (cond ((not (null? l)) (f (car l)) (for-each f (cdr l))))
)
(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

(define tree (list 1 (list 2 (list 3 4))))
(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(newline)
(display "leaves")
(newline)
(count-leaves tree)

; Exercise 2.25
; 1 3 (5 7) 9 --> cdr cdr car cdr car


; Exercise 2.26
; cons (list 1 2) (list 3 4)  results in (1 2) 3 4
; list (list 1 2) (list 3 4) results in (1 2) (3 4)
; append results in (1 2 3 4)

; Exercise 2.27
; Write a deep reverse function which takes in a list of lists
; and reverse the list as well as the sublists
(define (normal-rev x) 
    (define (iter curr res)
        (if (null? (cdr curr)) 
            (cons (car curr) res) 
            (iter (cdr curr) (cons (car curr) res)))
    )
    (iter x null)
)

(normal-rev (list 1 2 3 4 5 6 )) ; Should return (4 3) (2 1)
(define (deep-rev x)
    (define (iter curr res)
            (if (null? (cdr curr))
                (cons (if (pair? (car curr)) (deep-rev (car curr)) (car curr)) res)
                (iter (cdr curr) (cons (if (pair? (car curr)) (deep-rev (car curr)) (car curr)) res)))
        
    )
    (iter x null)
)

(deep-rev (list (list 1 2) 3 4 (list 5 6)))

; Exercise 2.29 Binary mobile
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch m) (car m))
(define (right-branch m) (car (cdr m)))
(define (branch-length b) (car b))
(define (branch-structure b) (car (cdr b)))
(define mobile (make-mobile (make-branch 1 2) (make-branch 3 4)))
(define mobile2 (make-mobile (make-branch 1 5) (make-branch 3 mobile)))
(define (total-weight m)
    (cond ((null? m) 0)
          ((not (pair? m)) m)
          (else (+ (total-weight (branch-structure (left-branch m))) (total-weight (branch-structure (right-branch m))))))
)

(total-weight mobile) ; 6
(total-weight mobile2) ; 11



; Exercise 2.30 square tree
; damn this is cool
(define (square-tree tree)
    (define (square x) (* x x))
    (map (lambda (sub-tree) (if (pair? sub-tree) (square-tree sub-tree) (* sub-tree sub-tree))) tree)
)
(square-tree (list 1 (list 2 3)))

(append (list 1 2) (list 3 4))


; Generic filter function
(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

; Generic accumulate function
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Exercise 2.33
(define (mymap p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))
(define (square x) (* x x))
(mymap square (list 1 2 3 5 6))
(define (myappend seq1 seq2)
    (accumulate cons seq2 seq1))
(myappend (list 1 2) (list 3 4))
; Note here that the y is like the running count
; and if we were folding left it would be x
(define (mylength sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))
(mylength (list 1 2 3 4))

; Exercise 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(accumulate / 1 (list 1 2 3)) ;  3/1 --> 3/2 --> 3/2/1 --> 3/2
(fold-left / 1 (list 1 2 3)) ; 1/1 --> 1/2 --> 1/2/3 --> 1/6 
(accumulate list null (list 1 2 3)) ; (1 (2 (3 ()))))
(fold-left list null (list 1 2 3)) ; (((() 1) 2) 3)
; for op to produce the same sequence for fold left and right
; you need op x y = op y x (commutative)

; Exercise 2.39
(define (acc-reverse sequence)
  (accumulate (lambda (x y) (append y (list x))) null sequence)) ; basically you want to keep adding to the head of the list here
(acc-reverse (list 1 2 3 4 5))


; Builds a linked list from low to high
(define (enumerate-interval low high)
  (if (> low high)
      null 
      (cons low (enumerate-interval (+ low 1) high))))

; Generate all the ordered pairs up to n
; First this says map some f on onto the sequence enumerate-interval 0-10
; That f converts EACH individual element of the sequence into a list of pairs
; We then accumulate all these lists into a single list of all the pairs
(define n 10)
(accumulate append
            null 
            (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 n)))

; Simpler example
(accumulate append null (list (list (list 1 2) (list 3 4)))) ; converts this to ((1 2) (3 4))
; Commonly used, lets call it flatmap
(define (flatmap proc seq)
  (accumulate append null (map proc seq)))


(define (permutations s)
  (if (null? s)                    ; empty set?
      (list null)                   ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(permutations (list 1 2 3))

; Exercise 2.42 N-Queeens
; How to place N queens on an NxN chess board so that no 2 queens are on the 
; same row, column or diagonal
; Complete the representation for board positions
; safe, adjoin-position 
; Board positions can be a list of lists and 1's indicate the presence of a queen
; i.e. A = ((0 0 1 0) (1 0 0 0) (0 0 0 1) (0 1 0 0)) is a valid 4x4 solution

; This function needs to take something like  A above minus the last column (rest-of-queens)
; and produce A  where new-row is the row with the queen in the last column
; k is index of that last row 
; Need to walk through every row of rest-of-queens and if the row index is new-row we add a 1
; otherwise we add a 0
; (define (for-each f l)
;   (cond ((not (null?  (f (car l)) (for-each f (cdr l))))
; )
(define (adjoin-position new-row k rest-of-queens)
    (cons new-row rest-of-queens)
)
; Check if the queen in the kth column is safe
; We already know its in a different column, just need to check diagonals 
; and rows
; Walk from the left hand side to the right
; This rest-of-queens should just be a list of the queen positions (index in the list indicates
; the row
(define (safe? k positions) 
    ; start from our queen and walk to the right
    ; return true or false if safe 
    (define queen (car positions))
    
    (define (iter curr left-diag right-diag)
        (cond ((null? curr) #t)
              ((= queen (car curr)) #f)
              ((= left-diag (car curr)) #f)
              ((= right-diag (car curr)) #f)
              (else (iter (cdr curr) (- left-diag 1) (+ right-diag 1))))
    )
    (iter (cdr positions) (- queen 1) (+ queen 1))
)
(define empty-board '())
(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 4)

