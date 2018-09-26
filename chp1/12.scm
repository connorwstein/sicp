; Section 1.2
(define (factorial n)
    (if (= n 1)
        1
        (* n (factorial (- n 1)))))
(factorial 5)

; Exercise 1.9
;(define (+ a b)
;    (if (= a 0)
;        b
;        (inc (+ (dec a) b))))

;(+ 4 5)
;(inc (+ 3 5))
;(inc (inc (+ 2 5))
;inc...
; recursive

;(define (+ a b)
;    (if (= a 0)
;        b
;        (+ (dec a) (inc b))))
; + 4 5
; + 3 6
; + 2 7
; ... iterative

; Exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (-x 1)
                  (A x (- y 1))))))

; (A 1 10) --> look at (A 1 3) first , notice that it is 8 = 2^3 i.e. A(1 x) = 2^x
; The rest can be done in a similar way

; Counting change
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
         ((or (< amount 0) (= kinds-of-coins 0)) 0)
         (else (+ (cc amount
                      (- kinds-of-coins 1))
                  (cc (- amount
                         (first-denomination kinds-of-coins))
                      kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))
(count-change 100)

; Exercise 1.11
; Recursive
(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

(f 4)
(f 3)

; Iterative
(define (fib-iter-step a b c a_old b_old c_old rem)
  (begin (display rem) (display c) (display c_old) (newline) (if (= rem 0)
      c
      (fib-iter-step b c (+ c (* 2 b) (* 3 a)) a b c (- rem 1)))))
(define (f-iter n)
  (fib-iter-step 0 1 2 0 1 2 (- n 2)))


(f-iter 3)
(f-iter 4)
(f-iter 5)
(f 5)


; Exercise 1.14
; current_amount = k1*d1 + k2*d2 + .. kn*dn
; k1 - [0, a/d1], k2 - [0, (a - k1*d1)/d2] ...
; # of (k1) options is proportional to a, # (k1, k2) tuples is proportional to a^2
; (k1, .. kn) ~ a^n in time

; Exercise 1.15
; a) 5 times, just substitute to see this
; b) logarithmic because subproblem is divided by 3 each time

; Exercise 1.16
(define (even? a)
  (= (remainder a 2) 0))
(define (square x) (* x x))
(define (fast-expt b n)
   (define (iter a b n)
     (cond ((= n 0) a)
           ((even? n) (iter a (square b) (/ n 2)))
           (else (iter (* a b) b (- n 1)))))
   (iter 1 b n))

(fast-expt 2 4)

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(gcd 28 16)

; If n is not prime, it must have a divisor which is < sqrt(n), so to determine if a number
; is prime, you only need to check numbers between 1 and sqrt(n), O(sqrt(n)) growth

; 1.21
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(smallest-divisor 199)
(prime? 104729)

;(if (prime? 100) (display "100 is prime") ((begin (newline) (display "100 is not prime"))))

; 1.22
(define (report-prime elapsed-time)
  (begin (display " *** ") (display elapsed-time) elapsed-time))
(define (start-prime-test n start-time)
  (if (prime? n)
      (begin (newline) (display (runtime)) (display start-time) "Prime!") "Not prime"))
      ;(report-prime (- (runtime) start-time))))
(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(timed-prime-test 104729)
(timed-prime-test 1299709)
(timed-prime-test 15485863)
(timed-prime-test 179424673)
;Using this procedure, write a procedure search-for-primes that checks the primality of consecutive odd integers in a specified range. Use your procedure to find the three smallest primes larger than 1000

; print all the prime numbers in a range
(define (search-for-primes low high) 
    (cond ((not (= low high))
          (if (prime? low)
            (begin (newline) (display low) (search-for-primes (+ low 1) high ))
            (search-for-primes (+ low 1) high )))))

(search-for-primes 0 100)

