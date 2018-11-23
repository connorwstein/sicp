; Exercise 3.38
; a) 4 possible values: 35, 40, 45, 50
; b) You could get a value of 60 for example if 
; b = 0.5*b  is interleaved with b = b-20 to leave b at 50 then b = b+10 is run

; If you have 2 processes with 3 ordered events, you end up
; with 20 different possible executions if interleaving is allowed
; because its 6 choose 3. 

; Exercise 3.39
; Here the multiplication is serialized but not the update of x 
; with that new product. This means that the update can be interleaved. and you can still end up with 100 for example (the increment runs
;in between the multiplation and the update with the product.


; Exercise 3.41
; There is no need to serialize a read-only op like get balance. Only one call to set! in each, so you could never get some kind of in-between balance. 

; Exercise 3.42
; No change to the concurrency structure by creating the serializers once versus on multiple calls.

; Exercise 3.45
; The internal serializers would never be able to run because 
; the external one already acquired the lock. The exchange procedure
; would just halt forever.

; Exercise 3.46
; If this is the test and set
(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true) false)))
; Then we can have interleaving in between reading the cell
; and writing to it. So for instance P1 could read 
; that its false, then P2 could also read that its false before
; P1 sets it to true and now we have both P1 and P2 thinking they
; have the lock, defeating the purpose.


; Exercise 3.48
; This resolves the issue because concurrent calls
; (serialized-exchange-no-deadlock a1 a2) and 
; (serialized-exchange-no-deadlock a2 a1) both result
; in the same serialization object s1(s2(ex)) so on exchange
; will run to completion before the other one does.
(define (serialized-exchange-no-deadlock account1 account2)
  (define (higher a1 a2) 
    (if (> (get-id a1) (get-id a2)) a1 a2))
  (define low (if (higher a1 a2) a2 a1))
  (define high (if (higher a1 a2) a1 a2))
  (let ((serializer1 (high 'serializer))
        (serializer2 (low 'serializer)))
    ((serializer1 (serializer2 exchange))
      account1 
      account2)))
