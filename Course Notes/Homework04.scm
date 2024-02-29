#lang racket
; Fourth Homework Set
; CSc 335
; Spring 2024



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Developments leading to proofs must be given for all programs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; 1.  Abelson and Sussman, Problems 1.11 and 1.12
;; 1.11:
;; Recursive process:
(define f-rec
  (lambda (n)
    (cond
      ((< n 3) n)
    (else
     (+ (f-rec (- n 1)) (* 2 (f-rec (- n 2))) (* 3 (f-rec (- n 3))))))))
    
;; Iterative process:
(define f
  (lambda (n)
    (f-iter 0 1 2 n)))
(define f-iter
  (lambda (a b c n)
    (cond
      ((= n 0) a)
      ((= n 1) b)
      ((= n 2) c)
      (else
       (f-iter b c (+ c (* 2 b) (* 3 a)) (- n 1))))))
(f 3)

;; 1.12:
      
; 2.  Write iterative and recursive scheme programs to test whether the digits in a non-negative
; integer are in increasing order.  For example, the digits of 12348 are in increasing order, while
; those of 12343 are not.