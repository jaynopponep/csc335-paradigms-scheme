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
     (+ (f-rec (- n 1))
        (* 2 (f-rec (- n 2)))
        (* 3 (f-rec (- n 3))))))))


;; Iterative process:
; We can create a table of three consecutive values (n-3, n-2, n-1) and show specific patterns
; We will say that n starts at n=0
; f(n) = n for n < 3
; n-3    n-2    n-1                             ; f(0) = 0
; 0      1      2     (n < 3)                   ; f(1) = 1
; 3(0)   2(1)   (2)   f(3) call -> 0+2+2=4      ; f(2) = 2
; 3(1)   2(2)   (4)   f(4) call -> 3+4+4=11     ; f(3) = 4
; 3(2)   2(4)   (11)  f(5) call -> 6+8+11=25    ; f(4) = 11
; 3(4)   2(11)  (25)  f(6) call -> 12+22+25=59  ; f(5) = 25
; So basically if we call f(5) for example, we add f(5-1)+2(5-2)+3(5-3)

; DESIGN IDEA: keep track of three numbers starting from 0, 1, 2 the basis integers
; Each iteration, we move up the next three sets. We will use an up-counter to track our stopping condition
; which is where count = n, in which we return the last computed value 
; Example, if we call (f 5):
; (f-iter 0 1 2 2)
; (f-iter 1 2 4 3)
; (f-iter 2 4 11 4)
; (f-iter 4 11 25 5) -> n=5, count=5, count=n

; Guess Invariants:
; i) n >= 0 is an integer
; ii) Stopping condition: count=n
; iii) In each iteration,
; n-3 becomes n-2: (n-3) = (n-3)+1
; n-2 becomes n-1: (n-2) = (n-2)+1
; n-1 becomes (+ n-1 (* 2 n-2) (* 3 n-3)): n-1 = (n-1 + (2(n-2)) + (3(n-3)))+1
; n-1 = (n-1 + (2((n-2)+1)) + (3((n-3)+1))) + 1
; n-1 = (n - 1 + (2(n-1)) + (3(n-2))) + 1
; n-1 = (n - 1 + 2n -2 + 3n - 6 + 1)
; n-1 = 6n - 8 = 2(3n-4)
; count becomes count+1


(define (f n)
  (define (f-iter n-3 n-2 n-1 count)
  (cond
    ((= count n) n-1)
    (else
     (f-iter n-2 n-1 (+ n-1 (* 2 n-2) (* 3 n-3)) (+ count 1)))))
  (cond ((< n 3) n)
        (else (f-iter 0 1 2 2))))
(f 5)


;; 1.12:
      
; 2.  Write iterative and recursive scheme programs to test whether the digits in a non-negative
; integer are in increasing order.  For example, the digits of 12348 are in increasing order, while
; those of 12343 are not.

(define (incorder? n)
  (cond ((< n 10) #t)
        ((> (modulo n 10) (modulo (quotient n 10) 10)) #t)
        ((< (modulo n 10) (modulo (quotient n 10) 10)) #f)
        (else (incorder? (quotient n 10)))))
(incorder? 1234569)

(define (incorder2? n)
  (define (inc-iter prev curr count)
    ((> prev curr) #f)
    ((< prev curr) #t)
    (else )))
    






   