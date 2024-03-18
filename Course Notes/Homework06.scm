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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Proofs are required only for 1.37 and 1.43

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Abelson and Sussman, Problems 1.29, 1.31, 1.34, 1.37 and 1.38

; 1.29 - Simpson's Rule:
(define (sum term a next b)
  (if (> a b) 0
      (+ (term a)
         (sum term (next a) next b))))
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (* (/ h 3)
     (+
      (f a)
      (* 4 (sum f a  
      (* 2 (
      (f b)
      


; Abelson and Sussman, Problems 1.41, 1.42, and 1.43


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Finally, how about working on the induction review problems I've posted?


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


