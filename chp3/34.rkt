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



