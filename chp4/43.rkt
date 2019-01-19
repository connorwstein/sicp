; Exercise 4.35
; Write a procedure an-integer-between which returns an integer between two bounds
(define (require p)
  (if (not p) (amb)))
(define (an-integer-between low high)
    (require (<= low high))
    (amb low (an-integer-between (+ low 1) high))
)

; Exercise 4.36
; First of all you can't just replace an-integer-between with an-integer-starting-from because one procedure
; takes two arguments and the other takes one, but I think the main thing is you need a way to ensure i <= j 
; (and obviously k will be > i, j)
; If we just pin down k first with an-integer-starting-from, then we can do a an-integer-between 1 and that k for j and then 1 and j for i
(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))
(define (a-pythagorean-triple)
  (let ((k (an-integer-starting-from 1)))
    (let ((j (an-integer-between 1 k)))
      (let ((i (an-integer-between 1 j)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))


; Exercise 4.38
; With the original contraints, you can see there is only one solution by evaluating where fletcher (F) can live (good 
; place to start because he has the most constraints on him) then looking at the cascading effects
; F on 5 --> not allowed directly by constraint
; F on 1 --> not allowed directly by constraint
; F on 2 --> forces S and C to be on 4,5 but that would be M > C 
; F on 3 --> forces S and C to be on 5,1 but C != 1 and C = 5 would break M > C
; F on 4 --> only one that works, forces S and C to 2,1 (C != 1, so C = 2 and S = 1), that leave B=3 (can't be 5) and M=5
; Now if we remove the constraint that F and S can't be adjacent, that opens up some new possibilities. 
; F = 3 still doesn't work, because C is forced to 5 which breaks M > C. F = 2 is now possible with M = 5 and C = 2  and 2 permutations
; for B and S, so we gained two solutions there. The original F = 4 also has new solutions - C is forced to 2 then, in addition to the original
; solution we can have S=5 F=4 M=3 C=2 B=1 and M=5 F=4 S=3 C=2 B=1. So total now is 5 solutions.


; This is cool its a little grammar parser
; word-list is a list starting with the type followed by instances of the type
; ex: '('noun 'cat 'dog ...)
; Then we walk through *unparsed* checking whether the next unparsed word is in the word-list, if so return 
; '(word-type word) 
(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))
