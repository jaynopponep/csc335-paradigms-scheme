﻿;; Quiz 2
;; CSc 335 Fall 2023
;; Section M
;;


;; Name and Last 4: __________________________________________________________________________________


;; This is a closed book, closed notes, no devices quiz.  Even the sight of an 
;; open phone or other device will result in your failing the quiz.

;; Design, prove and code an iterative function merge to input two
;; sorted nonnegative integers m and n, with output the sorted integer resulting
;; from merging m and n.  The definition of 'merge' is just as
;; it was in  your algorithms class; you may decide whether 'sorted' means digits
;; in non-decreasing or non-increasing order. Your procedure must preserve
;; multiplicities. You may assume that neither m nor n contain 0s. 

;; Example: for nondecreasing order, (merge 11234 11233345) = 1111223333445

;; Use only functions and numbers -- no lists, no strings, no vectors ...

;; Be sure to provide specifications for your functions. 

; Pre: m >= 0 & n >= 0 are integers
; Post: Return sorted values that are in m and n merged
; Design Idea: Best way is to start scanning each from the right and comparing
; the rightmost digits of m & n. If (mod n 10) < (mod m 10), add (mod m 10) to the solution.
; We will also need a counter for 10^(count) because everytime we are modding m or n,
; we must add (10^count*m) (in the case we are adding m to the solution)
; We will start count at 0 because when we are at the rightmost digit, we dont want to multiply
; the digit by 10.



