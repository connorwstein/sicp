#lang sicp
; Delay/force primitives
(define a (delay (+ 1 2)))
(force a)


; Exercise 3.50
; This is the generalized map, where it takes in n-streams (argstreams) 
; and applies an n-argument function to the first, second and so on elements of the lists
; to produce a single final stream 
; If there are no argument streams left (the list of argument streams is just a normal 
; list so if car argstreams is null) the we return the empty stream. 
; To extract the ith element from each stream we can map stream-car onto argstreams
; and to get everything after the ith element from each stream we can map stream-cdr onto argstreams.
; (define (stream-map proc . argstreams)
;   (if (null? (car argstreams))
;       the-empty-stream
;       (stream-cons 
;        (apply proc (map stream-car argstreams))
;        (apply stream-map
;               (cons proc (map stream-cdr argstreams))))))


; Exercise 3.51
;(define (show x)
;   (display-line x)
;   x)
; stream-enumerate-interval returns a stream where the car is 0 and the cdr 
; is promise to return 1-9. Stream-map will extract each value from the stream
; and apply the show to it
; (define x (stream-map show (stream-enumerate-interval 0 10)))
; (stream-ref x 5)
; (stream-ref x 7)


; Exercise 3.52
; (define sum 0)
; (define (accum x)
;   (set! sum (+ x sum))
;   sum)
; (define seq (stream-map accum (stream-enumerate-interval 1 20)))
; Sum is 1 here as the stream map builds a new stream by using cons-stream which only fires the first accum 
; (define y (stream-filter even? seq))
; Similarly only the first even number results in a firing of accum which actually mutates sum
; so its 6
; (define z (stream-filter (lambda (x) (= (remainder x 5) 0))
;                          seq))
; (stream-ref y 7) ; This is the 8th even number in the sequence 1, 3, 6, 10, 15... which is 136
; (display-stream z) ; just prints out all the numbers in seq which are divisible by 5 --> 10, 15, 45 ...
; since it goes all the way to the end, we end up with 210
; If there wasn't any memoization then display-stream would fire accum again (after stream-ref did) and the sum would be different.







