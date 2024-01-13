#lang racket
;
; Chapter 4 - Numbers Games
; Contents:
;

; add1 n: add 1 to n
(define (add1 n)
  (+ n 1))
; example:
(add1 '3)


; sub1 n: subtract 1 from n
(define (sub1 n)
  (- n 1))
; example:
(sub1 '5)
(sub1 '0) ; In the book, we only consider non-negative numbers. In practice, this will show -1


; zero?: checks if an atom is zero.
; example:
(zero? '0)
(zero? '1492)


; o+: a more manual approach to a function of adding two numbers
(define (o+ n m)
  (cond
    ((zero? m) n)
    (else (add1 (o+ n (sub1 m))))))


; o-: subtracting two numbers
(define (o- n m)
  (cond
    ((zero? m) n)
    (else (sub1 (o- n (sub1 m))))))


; tup: tuples, list of numbers, no atoms nor lists should be in the list.
; examples:
'(3 5 2 8)
'()


; addtup tup: builds a number by totaling all numbers in the list given as an argument
(define (addtup tup)
  (cond
    ((null? tup) 0)
    (else (o+ (car tup) (addtup (cdr tup))))))
; examples:
(addtup '(3 5 2 8))
(addtup '(15 6 7 12 3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The First Commandment (first revision):
; When recurring on a list of atoms, lat, ask two questions: (null? lat) and else.
; When recurring on a number, n, ask two questions: (zero? n) and else.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Fourth Commandment (first revision):
; Always change at least one argument while recurring.
; It must be changed to be closer to termination. The changing argument must be
; tested in the termination condition:
; when using cdr, test termination with null? and
; when using sub1, test termination with zero?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (o* n m)
  (cond
    ((zero? m) 0)
    (else (o+ n (o* n (sub1 m))))))
; breakdown of function:
; we simply treat o+ the same way we treated cons. an example is: (o* 12 3),
; we add 12 once and then we make a new argument 12 2. this adds 12 a second time,
; then we add 12 again, argument becomes 12 1. we add 12 one more time, the new argument
; is now 12 0. Since m=0, and our termination returns 0 if m=0, we recur back to all
; the 12s that we added in o+ n. We added 12 three times, or did 12x3; the function works.
; more intuitively it can be seen as:
; (o* 12 3) = 12 + (o* 12 2)
; = 12 + 12 + (o* 12 1)
; = 12 + 12 + 12 + (o* 12 0)
; = 12 + 12 + 12 + 0
;examples:
(o* 2 500)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Fifth Commandment:
; When building a value with o+, always use 0 for the value of the terminating line,
; for adding 0 does not change the value of an addition.
; When building a value with o*, always use 1 for the value of the terminating line,
; for multiplying by 1 does not change the value of a multiplication
; When building a value with cons, always consider () for the value of the terminating line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; RETURN TO PAGE 68 @SELF


