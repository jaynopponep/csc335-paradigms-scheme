#lang racket
; Sixth Homework Set
; CSc 335
; Spring 2024
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Here are some additional practice problems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Write a program to reverse the digits of an integer.  Thus 1234 --> 4321.
; Note that the output is a number!
; You will need to work out a precise specification: what if there are 0s?
; Pre: n >= 0 is an integer; no leading and trailing zeroes
; Post: Return n in reverse
(define (length n)
  (cond ((< n 10) 1)
        (else (+ 1 (length (/ n 10))))))
(define (make-num p q)
  (cond ((zero? p) q)
        ((zero? q) p)
        (else (+ p (* q (expt 10 (length p)))))))
(define (reverse-digs n)
  (cond ((< n 10) n)
        (else (make-num (reverse-digs (quotient n 10)) (modulo n 10)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Here are some problems for you if you would like to work ahead.  You will
; want to read the associated sections in Abelson and Sussman before starting. 


; Abelson and Sussman, Problems 1.29, 1.31, 1.34, 1.37 (proof) and 1.38

; 1.29 - Simpson's Rule:
(define (sum term a next b)
  (if (> a b) 0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson f a b n)
  (simp-aux f a b n (/ (- b a) (* n 1.0) )))

(define (simp-aux f a b n h)
  (define (term k)
    (cond
      ((= 0 k) (f a))
      ((odd? k) (* 4 (f (+ a (* k h)))))
      ((even? k) (* 2 (f (+ a (* k h )))))
      ((= n k) (f (+ a (* k h))))))
  (define (next k)
    (+ k 1))
  (* (/ h 3.0)
     (sum term 0 next n)))


; 1.31
; product recursion
(define (product term a next b)
  (if (> a b) 1
      (* (term a)
         (product term (next a) next b))))
(define (next k)
  (+ k 1))
; define factorial in terms of product
(define (factorial n)
  (product * 1 next n))
(factorial 7)
; define Wallis' product
(define (wallis n)
  (define (term k)
    (cond ((odd? k) (* 1.0 (/ (+ k 1) (+ k 2))))
          (else (* 1.0 (/ (+ k 2) (+ k 1))))))
  (product term 1 next n))
(* 4 (wallis 99999))

; 1.34 
; (f f) results in (f 2) which is (2 2) which makes no sense, throws an error

; 1.37


; 1.38

; Abelson and Sussman, Problems 1.41, 1.42, and 1.43 (proof)




