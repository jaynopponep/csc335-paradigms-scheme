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
;;; => b -> a+1 ; b -> 4

; (+ a b (* a b))
;;; => a=3, b=4, (* a b)=12: 3+4+12 = 19

; (= a b)
;;; => a=b -> 3=4? returns #f

;(if (and (> b a) (< b (* a b)))
;    b
;    a)
;;; => (> b a)=#t, (* a b)=12, (< b 12)= #t, (and (#t) (#t)) yields #t, so we return b (4). However, if this doesn't work out, return a (3) instead.

;(cond ((= a 4) 6)
;      ((= b 4) (+ 6 7 a))
;      (else 25))
;;; => a!=4, we don't return 6.
;;; => b=4, so we return (+ 6 7 3), which is 16.
;;; => return 16

;(+ 2 (if (> b a) b a))
;;; b>a -> 4>3 -> #t, so we return b (not a!). b becomes second argument of procedure +. (+ 2 b) -> (+ 2 4) -> 6. we return 6

;(* (cond ((> a b) a)
;         ((< a b) b)
;         (else -1))
;   (+ a 1))
;;; a!>b, next condition: a<b is true, return b. b becomes first argument of procedure *. (* 4 (+ 3 1)) -> (* 4 4) -> 16


; Exercise 1.2:
(/ (+ 5 4 (- 2 (- 3 (+ 6 4/5)))) (* 3 (- 6 2) (- 2 7)))


; Exercise 1.3
(define two-large
  (lambda (a b c)
    (cond
      ((and (< a b) (< a c) (+ (* b b) (* c c))))
      ((and (< b a) (< b c) (+ (* a a) (* c c))))
      ((and (< c a) (< c b) (+ (* a a) (* b b)))))))

(two-large 3 4 5)
; Exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
(a-plus-abs-b -1 2)
; the procedure above makes sure to add the absolute values of both a and b. If b happens to be a negative, the procedure will use -, which means this will be - a b.
; Since the negatives will cancel out and just add a + b (as a positive number), a is added to the absolute value of b.
; In the case that a is a negative number, it will still use the addition operator, but this means just subtracting.


; Exercise 1.5
; APPLICATIVE-ORDER EVALUATION: The interpreter will evaluate the parameter 0, which is just a 0, then evaluate the (p), which then moves to evaluate the value in (define (p) (p)).
; Once this is done, it will finally start interpreting the define (test x y) function.
; NORMAL-ORDER EVALUATION: The interpreter would immediately proceed to interpreting the (define (test x y) function, and then check if x is = 0, then it returns 0. If it is not, it will then check the parameter y,
; then it will see that the parameter passed for y is (p), and then evaluate the value in (define (p) (p)), and then finally return (p) since x is not equal to 0.


