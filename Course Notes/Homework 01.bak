#lang scheme
; Homework 1
; Spring 2024

; Review the notes for classes 1 and 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; You should read Sections 1.1.1 through 1.1.6, and solve Exercises
;; 1.1 through 1.5, in Abelson and Sussman.

;; To prepare for our next classes, read Sections 1.1.7 and 1.1.8, as well
;; as Section 1.2.1, all in Abelson and Sussman.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 1.1: What is the result printed by the interpreter in response to each expression below?
; 10
;;; => 10
; (+ 5 3 4)
;;; => 12
; (- 9 1)
;;; => 8
; (/ 6 2)
;;; => 3
; (+ (* 2 4) (- 4 6))
;;; => 6
; (define a 3)
;;; => a -> 3
; (define b (+ a 1))
;;; => b -> a+1
; (+ a b (* a b))
;;; => a is added to b which is then added to (* a b)
; (= a b)
;;; => a=b

;(if (and (> b a) (< b (* a b)))
;    b
;    a)
;;; => if the following are BOTH true, b is printed. or else, a.
;;; => the following: b > a, b < (a*b)

;(cond ((= a 4) 6)
;      ((= b 4) (+ 6 7 a))
;      (else 25))
;;; => conditions are listed. if a=4, 6 is printed.
;;; => if a!=4, ask if b=4. If so, 6+7+a is printed.
;;; => if all above are false, print 25

;(+ 2 (if (> b a) b a))
;;; if b > a, if procedure returns b & a, which is then added to 2.
;;; if b is not > a, then most likely an error??

;(* (cond ((> a b) a)
;         ((< a b) b)
;         (else -1))
;   (+ a 1))
;;; we check each condition. check if a > b, if so return a.
;;; if a < b, return b


