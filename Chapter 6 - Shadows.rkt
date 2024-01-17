#lang racket
;
; Chapter 6 - Shadows
; Contents: The Seventh & Eighth Commandment, numbered?, prefix value, value, etc.
;
; REDEFINING:
; atom?
(define (atom? x)
    (and (not (pair? x)) (not (null? x))))
; exponent:
(define (^ n m)
  (cond
    ((zero? m) 1)
    (else (* n (^ n (sub1 m))))))

; arithmetic expressions:
; an atom (including numbers) or two arithmetic expressions combined by +, *, ^
; no parentheses around arithmetic expressions. we call these representations of what's in the parentheses:
; example: (n+3) is a representation of n+3

; numbered?: function that determines whether a representation of an arithmetic expression contains only
; numbers besides the operations (+, *, ^)
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (numbered? (car aexp))
            (numbered?
             (car (cdr (cdr aexp)))))))))
;example:
(numbered? '(1 + 1))
; breakdown: we simply need to check if it is either an atom or a number. if so then it is numbered.


; value: returns the arithmetic/numerical value or the entire arithmetic expression with operations
(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) (quote +))
       (+ (value (car nexp))
          (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) (quote *))
       (* (value (car nexp))
          (value (car (cdr (cdr nexp))))))
      (else
       (^ (value (car nexp))
          (value
           (car (cdr (cdr nexp)))))))))
;example:
(value '(2 ^ 4))

; help functions: 1st-sub-exp (car of the cdr), 2nd-sub-exp (the one after) this applies for
; math expressions like (* 2 3)
(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value-prefix
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) (quote +))
       (+ (value-prefix (1st-sub-exp nexp))
          (value-prefix (2nd-sub-exp nexp))))
      ((eq? (operator nexp) (quote *))
       (* (value-prefix (1st-sub-exp nexp))
          (value-prefix (2nd-sub-exp nexp))))
      (else
       (^ (value-prefix (1st-sub-exp nexp))
          (value-prefix (2nd-sub-exp nexp)))))))
;example:
(value-prefix '(* 2 4))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Seventh Commandment:
; Recur on the subparts that are of the same nature:
; - On the sublists of a list
; - On the subexpressions of an arithmetic expression.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Eighth Commandment:
; Use help functions to abstract from representations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; *shadows rant*:
;sero? like zero?
(define sero?
  (lambda (n)
    (null? n)))
;edd1 like add1
(define edd1
  (lambda (n)
    (cons (quote ()) n)))
;zub1 like sub1
(define zub1
  (lambda (n)
    (cdr n)))
;a+ like o+
(define a+
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else (edd1 (a+ n (zub1 m)))))))
;redefine lat?
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))
(lat? '(1 2 3)) ;yields true
(lat? '((()) (()()) (()()()))) ; yields false
; Is that bad? You must beware of shadows.