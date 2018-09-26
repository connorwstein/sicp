; 1.3 Higher order procedures
; Generic pattern for summation
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (cube a) (* a a a))
(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (loop n) 
    (if (= n 0) (display "done") ((begin (display n) (newline) (loop (- n 1))))))
(loop 5)
