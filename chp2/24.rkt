#lang racket

; Primitives required 
(define *the-table* (make-hash))
(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value))
(define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f))
(define (square x) (* x x))

; (define (make-from-real-imag x y)
;   (make-from-real-imag-rectangular x y))
; (define (make-from-mag-ang r a)
;   (make-from-mag-ang-polar r a))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a) 
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y) 
  (attach-tag 'polar
               (cons (sqrt (+ (square x) (square y)))
                     (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

; This is not ideal because we need to have these 
; conditional statements everywhere and when you add a new representation
; you need to update all of them
; (define (real-part z)
;   (cond ((rectangular? z) 
;          (real-part-rectangular (contents z)))
;         ((polar? z)
;          (real-part-polar (contents z)))
;         (else (error "Unknown type -- REAL-PART" z))))
; (define (imag-part z)
;   (cond ((rectangular? z)
;          (imag-part-rectangular (contents z)))
;         ((polar? z)
;          (imag-part-polar (contents z)))
;         (else (error "Unknown type -- IMAG-PART" z))))
; (define (magnitude z)
;   (cond ((rectangular? z)
;          (magnitude-rectangular (contents z)))
;         ((polar? z)
;          (magnitude-polar (contents z)))
;         (else (error "Unknown type -- MAGNITUDE" z))))
; (define (angle z)
;   (cond ((rectangular? z)
;          (angle-rectangular (contents z)))
;         ((polar? z)
;          (angle-polar (contents z)))
;         (else (error "Unknown type -- ANGLE" z))))

(define (attach-tag type-tag contents)
    (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

; Note get and put are not implemented until chapter 3
; This solves the issue of having to name everything with its suffix as a type
; to avoid name conflicts and simplifies the process of adding a new type 
; because we only need to add it to the map via a new package
; Our table contains multiple procedures with the same name, but different 
; contents depending on the type
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

; Now these selectors do not change at all when we add a new representation
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))


; Exercise 2.73
; They describe converting the symbolic differentiation program into a simpler one
; which does this table lookup thing for determining which function to call
; A) Why can't they assimilate number? and same-variable? into the data directed approach:
; if its a number or same-var it wont have operator and operands
; D) I this case the operator is the "+-/*" rather than derivative and the type is derivative. 
; I don't think there is much change aside from the put calls will be different. We still have the same procedures
; in the map but they are just indexed differently 

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; Need to put a bunch of procedures, should
; work regardless of expression representation
; goal is to put 'deriv (operator exp) function
; example 'deriv + deriv-plus
(define (install-symbolic-diff)
  ;; internal procedures
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (make-sum a1 a2) 
      (cond ((=number? a1 0) a2)
            ((=number? a2 0) a1)
            ((and (number? a1) (number? a2)) (+ a1 a2))
            (else (list '+ a1 a2))))
  ;; interface to the rest of the system
  (put 'deriv '+ (lambda (exp var) 
            (make-sum (deriv (addend exp) var)
                      (deriv (augend exp) var))))
  (define (base x) (car x)) 
  (define (exponent x) (cadr x)) 
    (define (make-product m1 m2)
      (cond ((or (=number? m1 0) (=number? m2 0)) 0)
            ((=number? m1 1) m2)
            ((=number? m2 1) m1)
            ((and (number? m1) (number? m2)) (* m1 m2))
            (else (list '* m1 m2))))
    (define (make-exponentiation b e)
        (cond ((= e 0) 1)
              ((= e 1) b)
              ((and (number? b) (number? e)) (exp b e))
              (else (list '** b e)))
    ) 
    (define (multiplier p) (car p))
    (define (multiplicand p) (cadr p))

  ;; interface to the rest of the system
  (put 'deriv '* (lambda (exp var)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp)))))

  (put 'deriv '** (lambda (exp var) 
            (make-product (exponent exp) 
            (make-product (make-exponentiation (base exp) (make-sum (exponent exp) -1)) 
                                        (deriv (base exp) var)))))
)

; Note doesn't support arbirtary number of operands
(install-symbolic-diff)
(deriv '(+ x 2) 'x)
(deriv '(+ x x) 'x)
(deriv '(+ (* 3 (** x 2)) (* 2 x)) 'x) ; Works!


; Exercise 2.74
; Each divisions employee records contain a single file containing records keyed by employees names
; The structure of the records varies by division. The records can contain keyed records themselves,
; keyed by address or salary for example
; Implement a get-record procedure which is applicable to any file
; Idea is to have a generic get-record which calls the appropriate get-record depending on the division
; type. Need to know the type of the key
; Assume tag is just a division number prepended to the actual key

; Returns a typed record (to aid with decoding)
; (define (get-record key file)
;     (attach-tag (division file) (get 'get-record (division file)) (contents key))
; )
; (put 'get-record 1 (define (div-1-get-record key) (...) ))
; (get-record (attach-tag 1 "bob"))

;b) Implement for headquarters a get-salary procedure that returns the salary information from a 
; given employee's record from any division's personnel file. How should the record be structured in order to make this operation work?
; Say the record is a cons of type and contents
; The record is typed, use the type to get the get-salary function for that type, then use it to lookup the 
; the contents
; (define (get-salary record) 
;     (let ((type (car record))
;           (contents (cdr record)))
;         ((get 'get-salary type) contents))
; )

;c) Implement for headquarters a find-employee-record procedure. This should search all the divisions' files for the record of a given employee and return the record. Assume that this procedure takes as arguments an employee's name and a list of all the divisions' files
; We have to check each division 
; using get-record which based on the file type will know how to search it
; (define (find-employee-record name files) 
;     (define (iter record rem-files)
;         (if (null? (car rem-files)) 
;             record
;             (iter (get-record name) (cdr rem-files))))
;     (iter null files)
; )

; d) Need to expose a get-record function for that division which knows about their structure

; Exercise 2.76 
; Generic operations with explicit data dispatch: Great for adding new operations because we call a specific type-op function based on the object
; type 
; Message-passing: Great for adding new types as all the operating information is contained within the type, but adding new operations 
; results in changing every type 
; Data-directed: Really is the solution to both problems - we have a table of operations and types and to extend either just involves
; updating the table
