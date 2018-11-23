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


; Exercise 3.21
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue)))) 
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue))) 
; Problem is if the front pointer points to null there is nothing in the queue (the rear is irrelavent)
; but lisp will still print the rear. Lets just walk through the queue
(define (print-queue q) 
	(if (null? q) 
		(display "done") 
		(begin (display (car q)) (print-queue (cdr q))))
)
(define q1 (make-queue))
(insert-queue! q1 'a)
(print-queue q1)
(insert-queue! q1 'b)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)

; Exercise 3.22
(define (make-queue-internal)
  (let ((front-ptr '())
        (rear-ptr '()))
	(define (insert item) 
	  (let ((new-pair (cons item '())))
		(cond ((empty-queue?)
			   (set! front-ptr new-pair)
			   (set! rear-ptr new-pair))
			  (else
			   (set-cdr! rear-ptr new-pair)
			   (set! rear-ptr new-pair)))))
	(define (delete) 
		(if (empty-queue?) (display "Queue empty") (set! front-ptr (cdr front-ptr))))
	(define (print-queue) 
		(display front-ptr)
	)
	(define (empty-queue?) (null? front-ptr))
    (define (dispatch m) 
		(cond ((eq? m 'insert) insert)
			  ((eq? m 'empty-queue) empty-queue?)
			  ((eq? m 'print-queue) print-queue)
			  ((eq? m 'delete) delete)
			  (else (display "unknown"))))
    dispatch))
(define q (make-queue-internal))
((q 'empty-queue))
((q 'insert) 'a)
((q 'empty-queue))
((q 'insert) 'b)
((q 'print-queue))
((q 'delete))
((q 'print-queue))
((q 'delete))
((q 'print-queue))
((q 'insert) 'c)
((q 'print-queue))

; Exercise 3.23
; How to represent a deque using pairs?
; front-deque and rear-deque, and mutators front-insert-deque!, rear-insert-deque!, front-delete-deque!, and rear-delete-deque!
(define (make-dq) (cons '() '()))
(define (front-dq-ptr dq) (car dq))
(define (rear-dq-ptr dq) (cdr dq))
(define (set-front-dq-ptr! queue item) (set-car! queue item))
(define (set-rear-dq-ptr! queue item) (set-cdr! queue item))
(define (empty-dq? dq) (null? (front-dq-ptr dq)))
; each item --> [back [item next]]
(define (prev item) (car item))
(define (next item) (cddr item))
(define (item _item) (cadr _item))
(define (set-prev! item prev) (set-car! item prev))
(define (set-next! item next) (set-cdr! (cdr item) next))

; Need to update both front and back pointers, still special case 
; for empty 
(define (rear-insert-dq dq item) 
	(let ([new-item (cons (cons '() item) '())])
	(cond ((empty-dq? dq) 
		   (set-front-dq-ptr! dq new-item)
		   (set-rear-dq-ptr! dq new-item))
		  (else ; set the new-items back pointer to rear, set the rear cdr
		   (set-prev! new-item (rear-dq-ptr dq))
		   (set-rear-dq-ptr! new-item))))
)
(define dq (make-dq))
(rear-insert-dq dq 'a)
dq
; Other functions similar, just manipulate prev and next appropriately


; Section 3.3.3 Tables

; Exercise 3.24
; assoc returns the record which has the given key 
(define (assoc same-key? key records)
  (cond ((null? records) false)
        ((same-key? key (caar records)) (car records))
        (else (assoc key (cdr records)))))
(define (make-table same-key?)
  (let ((local-table (list '*table*))) ; *table is a dummy item in the table backbone 
    (define (lookup key-1 key-2)
      (let ((subtable (assoc same-key? key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc same-key? key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc same-key? key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc same-key? key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
(display "table stuff")
(newline)
(define t (make-table eq?))
((t 'insert-proc!) 't1 'a 1)
((t 'lookup-proc) 't1 'a)
