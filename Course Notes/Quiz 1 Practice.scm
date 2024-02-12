#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quiz 1 Prep -- Some Very Basic Number Stuff For Review
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; positional notation -- eg -- 123 = 1 x 10^2 + 2 x 10^1 + 3 x 10^0

;; (1) how would you extract the rightmost 2 digits from 123?

;;;   Hint: use the primitives modulo or remainder, and expt -- as well as + and *
;;;;  (1.1)    are scheme's modulo and remainder identical functions?
(define rightmosttwo
  (lambda (x)
    (modulo x 100)))
(rightmosttwo 123)


;; (2)  how would you extract the leftmost digit from 123?

;;;   Hint: use the quotient and expt primitives
(define leftmost
  (lambda (x)
    (quotient x 100)))
(leftmost 123)

;; (3) write a scheme expression to recover 123 from 1 and 23
(define recover
  (lambda (x y)
    (+ (* x 100) y)))
(recover 1 23)

;; (4) what is the value of this expression?
;;; (+ (* (quotient 123 (expt 10 2)) (expt 10 2)) (remainder 123 (expt 10 2)))

; (+ (* (quotient 123 100) 100) (remainder 123 100))
; (+ (* 1 100) 23)
; (+ 100 23)
; 123

;; (5) given nonnegative integers m and n, what scheme expression would you use to compute the number formed by
;; placing n to the left of m?  For example, how would you compute 4567123 from 123 and 4567?
(define inc
  (lambda (x)
    (+ x 1)))

(define length
  (lambda (x)
    (cond
      ((< x 10) 1)
      (else
       (inc (length (/ x 10)))))))

(define n-m
  (lambda (m n)
    (+ m (* n (expt 10 (length m))))))
(n-m 123 4567)

;; (6) can you use scheme's log function to count the digits in a nonnegative integer m?

;;; Hint: log is a scheme primitive computing natural logarithms
(define digitcounter
  (lambda (x)
    (+ (floor (log x 10)) 1)))

;(define digitcounter
 ; (lambda (x)
  ;  (+ (floor (+ (log x 10) 1e-10)) 1)))
; alternative method^ but still some problems, not 100% 


;;  (7) can you think of a way to count the digits in a nonnegative integer n without using log, and without using
;;  recursion (this implies: without iteration either, as iteration is a special kind of recursion)
;; EDIT: PRECONDITION: n is 6 digit integer
; POSTCONDITION: Output length of n


;; (8) how would you  count the digits in a negative integer?



;; (9) write a scheme function -- no recursion! -- which counts the digits in an integer
;refer to question 6^
(digitcounter 999)




