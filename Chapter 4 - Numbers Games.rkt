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


(define (o+ n m)
  (cond
    ((zero? m) n)
    (else (add1 (o+ n (sub1 m))))))

; RETURN TO PG60 @SELF