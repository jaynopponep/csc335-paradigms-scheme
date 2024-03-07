#lang racket
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
; the rightmost digits of m & n. If right-most-n < right-most-m, add right-most-m to the solution.
; We will need to create make-num function that will add [p][q] together with the use of length of q to identify how many times to multiply p by 10 and then adding to q
; We will start count at 0 because when we are at the rightmost digit, we dont want to multiply
; the digit by 10.

;; GI :  original m = [m-nyp][m-ap] and m, m-nyp and m-ap are sorted
;;       original n = [n-nyp][n-ap] and n, n-nyp and n-ap are sorted
;;       ap is the sorted merge of m-ap and n-ap

;;       &&

;;       any digit in m-nyp is <= any digit in rsf

;;       &&

;;       any digit in n-nyp is <= any digit in rsf

; simpler example: (merge 1123 124)

;Helper functions:
; Pre: n>=0 is an integer
; Post: Return length of n
(define (length n)
  (cond ((< n 10) 1)
        (else (+ 1 (length (/ n 10))))))

; make-number function that utilizes length to figure out our index/step
; Pre: p >= 0, q >= 0 are integers
; Post: Return [p][q]
(define (make-num p q)
  (cond
    ((= q 0) p)
    (else 
     (+ q (* p (expt 10 (length q)))))))

; right-most digit for readability:
; Pre: n >= 0
; Post: return the right most digit of n. This can be 0, which means that the rightmost digit is itself
(define (rmd n)
  (modulo n 10))

(define (lmd n)
  (quotient n (expt 10 (- (length n) 1))))

(define (merge m n)
  (cond 
    ((< (modulo m 10) (modulo n 10)) (merge-iter m (quotient n 10) (modulo m 10) (modulo (quotient n 10) 10) (modulo n 10)))
    (else (merge-iter (quotient m 10) n (modulo (quotient m 10) 10) (modulo n 10) (modulo m 10)))))
(define (merge-iter m n rm-m rm-n ap)
  (cond
    ((zero? m) (make-num n ap))
    ((zero? n) (make-num m ap))
    ((>= rm-m rm-n) (merge-iter (quotient m 10) n (modulo (quotient m 10) 10) rm-n (make-num rm-m ap)))
    ((< rm-m rm-n) (merge-iter m (quotient n 10) rm-m (modulo (quotient n 10) 10) (make-num rm-n ap)))))
(merge 1123 124)





