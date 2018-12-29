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
