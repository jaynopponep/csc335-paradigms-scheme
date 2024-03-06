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

; Pre: n>=0 is an integer
; Post: Return true if the digits are in increasing order; return false if the digits are in decreasing order

; Design Idea: Keep track of two integers from the end of the integer n. One will be right, which will be the final digit of the integer
; and one will be left, which is the direct left of the final digit. After each iteration, we will move left to the left one digit, right to the left one digit.
; At iteration, we must compare left and right. If left > right; return false because this would not be in increasing order; otherwise, we return true.
; We also would like to have another parameter to keep track of the NYP in order to check if (modulo NYP 10) = 0, then we will return #t because there are no more digits in NYP
; and all are in AP. All are processed

; Termination Idea: stop when NYP = 0, yielding #t. if left > right, we also stop here and return #f immediately

; GI: Since we are using NYP and AP, we can just say the guess invariant is:
; n = NYP * 10^(#digs in AP) + AP
; n is in sorted order if NYP is in sorted order and AP is in sorted order.
; Throughout each iteration, the AP consists of digits in increasing order.
; EDIT (4/5/24): We'll take AP as a "virtual variable" since it is not directly used in code like NYP, but is defined in our GI


; STRONG ENOUGH? - Assuming AP represents all the digits in increasing order, when NYP reaches 0 (NYP=0), n=AP,
; where instead of returning AP, we return either true or false whether AP is entirely increasing order and this is true,
; it is in increasing order when NYP reaches 0. 
; WEAK ENOUGH? - At the first call, we expect NYP = n because we have not processed any digits by comparing
; When we check this with our GI, our AP should be = 0, n = NYP * 10^0 (0 since no digits are processed in AP) + AP -> n = NYP * 1 TRUE!
; PRESERVES? - After our first call, assume that we are not at either of our stopping conditions.
; For NYP, we pass NYP without the rightmost digit, move 'left' one digit to the left, and 'right' one digit to the left.
; This means that we have one new processed digit in AP, meaning 1 less than originally from NYP. Assume NYP was originally 1234, now it is 123.
; n = 123 * 10^(1) + 4 => n = 1230 + 4 => n = 1234. It preserves.

; EDIT (4/5/24): we can possibly optimize the program by removing left since left represents the farthest right of nyp
; left removed. we extract rightmost digit of NYP directly and use it to compare to 'right'
(define (incorder2? n)
  (define (inc-iter nyp right)
    (cond ((zero? nyp) #t) 
          ((> (modulo nyp 10) right) #f)
          (else (inc-iter (quotient nyp 10) (modulo nyp 10)))))
    (cond ((< n 10) #t)
          (else (inc-iter (quotient n 10) (modulo n 10)))))

(incorder2? 1589)
(incorder2? 9876)
(incorder2? 12034)
(incorder2? 145556)






   