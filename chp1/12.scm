;#lang sicp
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
        (else (A (- x 1)
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

; Exercise 1.12
; Write a procedure which computes the elements of pascals triangle by means of a recursive process
; Note we mean recursive process not procedure
(define (pascals x y ) (
    cond ((or (<= x 0) (<= y 0) (= x y)) 1) 
          (else (+ (pascals (- x 1) (- y 1)) (pascals (- x 1) y)))
    )
)
(pascals 1 1) ; 1 choose 1 should be 1
(pascals 3 2); 3 choose 2 should be 3
(pascals 23 5); 23 choose 5 should be  33649


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

; Exercise 1.17
(define (double x) (* x 2)) ; could be shift operators which would be fast
(define (halve x) (/ x 2))
(define (fast-prod x y)
  (cond ((or (= x 0) (= y 0)) 0)
        ((= x 1) y)
        ((= y 1) x)
        (else (if (even? y)
            (double (fast-prod x (halve y)))
            (+ x (fast-prod x (- y 1)))))
       )
  )
(even? 10)
(fast-prod 10 2)
(fast-prod 11 13)



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
  (begin (display " *** ") (display elapsed-time) (newline)))
(define (start-prime-test n start-time)
  (if (prime? n)
      ;(begin (newline) (display (runtime)) (display start-time) "Prime!") "Not prime"))
      (report-prime (- (runtime) start-time))))
(define (timed-prime-test n)
  (start-prime-test n (runtime)))


;Using this procedure, write a procedure search-for-primes that checks the primality of consecutive odd integers in a specified range. Use your procedure to find the three smallest primes larger than 1000

; print all the prime numbers in a range
(define (search-for-primes low high) 
    (cond ((not (= low high))
          (if (prime? low)
            (begin (newline) (display low) (search-for-primes (+ low 1) high ))
            (search-for-primes (+ low 1) high )))))

;(search-for-primes 0 100)

; 1.23
; Improve prime test by skipping even numbers
(define (next n) (if (= n 2) 3 (+ n 2)))
(define (smallest-divisor-2 n)
  (find-divisor-2 n 2))
(define (find-divisor-2 n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (prime2? n)
  (= n (smallest-divisor-2 n)))
(define (start-prime-test-2 n start-time)
  (if (prime2? n)
      ;(begin (newline) (display (runtime)) (display start-time) "Prime!") "Not prime"))
      (report-prime (- (runtime) start-time))))
(define (timed-prime-test-2 n)
  (start-prime-test-2 n (runtime)))

; Speed up factor is not 2
; not exactly sure why
(begin (newline) (display "not skipping even") (newline))
(timed-prime-test 104729)
(timed-prime-test 1299709)
(timed-prime-test 15485863)
(timed-prime-test 179424673)

(begin (newline) (display "skipping even") (newline))
(timed-prime-test-2 104729)
(timed-prime-test-2 1299709)
(timed-prime-test-2 15485863)
(timed-prime-test-2 179424673)

; 1.24
; fermats theorem is if a^n mod n = a for all a < n then n is prime
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))        
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (start-prime-test-fermat n start-time)
  (if (fast-prime? n 100)
      ;(begin (newline) (display (runtime)) (display start-time) "Prime!") "Not prime"))
      (report-prime (- (runtime) start-time))))

(define (timed-prime-test-fermat n)
  (start-prime-test-fermat n (runtime)))

(begin (newline) (display "Fermats") (newline))
; with fermats method the speed doesn't take much longer for larger primes 
(timed-prime-test-fermat 1009) 
(timed-prime-test-fermat 1013) 
(timed-prime-test-fermat 1019) 
(timed-prime-test-fermat 10007) 
(timed-prime-test-fermat 10009) 
(timed-prime-test-fermat 10037) 
(timed-prime-test-fermat 100003) 
(timed-prime-test-fermat 100019) 
(timed-prime-test-fermat 100043) 
(timed-prime-test-fermat 1000003) 
(timed-prime-test-fermat 1000033) 
(timed-prime-test-fermat 1000037)
(timed-prime-test-fermat 1000000007) 
(timed-prime-test-fermat 1000000009) 
(timed-prime-test-fermat 1000000021) 
(timed-prime-test-fermat 10000000019) 
(timed-prime-test-fermat 10000000033) 
(timed-prime-test-fermat 10000000061) 
(timed-prime-test-fermat 100000000003) 
(timed-prime-test-fermat 100000000019) 
(timed-prime-test-fermat 100000000057) 
(timed-prime-test-fermat 1000000000039) 
(timed-prime-test-fermat 1000000000061) 
(timed-prime-test-fermat 1000000000063) 
; 1.25
; Can we use this expmod instead 
; (define (fast-expt b n)
;   (cond ((= n 0) 1)
;         ((even? n) (square (fast-expt b (/ n 2))))
;         (else (* b (fast-expt b (- n 1))))))
; It would not because there are huge intermediate results which are expensive to compute


; 1.26 explain the difference between these two expmod implementations
; one is o(n) and one is o(log(n))

; This is o(logn)
; (define (expmod base exp m)
;   (cond ((= exp 0) 1)
;         ((even? exp)
;          (remainder (square (expmod base (/ exp 2) m))
;                     m))
;         (else
;          (remainder (* base (expmod base (- exp 1) m))
;                     m))))        
; This is o(n) because the two recursive calls ruin our logarithmic 
; complexity. Just draw out a tree of the recursive calls and you will see.
; (define (expmod base exp m)
;   (cond ((= exp 0) 1)
;         ((even? exp)
;          (remainder (* (expmod base (/ exp 2) m)
;                        (expmod base (/ exp 2) m))
;                     m))
;         (else
;          (remainder (* base (expmod base (- exp 1) m))
;                     m))))

; 1.27 try every a < n
; start from 0 all the way up to n-1
; return false 
(define (carmichael n)
  (define (iter a)
    (cond ((= a n) #t)
          ((= (expmod a n n) a) (iter (+ a 1)) )
          (else #f))
    )
  (iter 0))

; 1105 passes the fermat test but is not prime 
(carmichael 1105)
(prime? 1105)
(carmichael 1013)
(prime? 1013)

; 1.28 Miller rabin test cannot be fooled by these
; carmichael numbers
; You when you do the square you need to check and see if you have discovered a non-trivial
; square root of 1 mod n (a number not equal to 1 or n-1 whose square is equal to 1 mod n)
; i.e. x^2 mod m = 1 and x != 1 and x != m-1
; make this return 0 if it does find such a value
(define (expmod-mr base exp m)
  (define (check-square x)
   (define (check-square-helper x square)
    (if (and (= square 1) (not (= x 1)) (not (= x (- m 1)))) 0 square))
    (check-square-helper x (remainder (square x) m)))
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (check-square (expmod-mr base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod-mr base (- exp 1) m))
                    m)))
)

(define (carmichael-2 n)
  (define (iter a)
    (cond ((= a n) #t)
          ((= (expmod-mr a n n) a) (iter (+ a 1)) )
          (else #f))
    )
  (iter 0))
; Can't fool me now!
(carmichael-2 1105)
(carmichael-2 1013)
