#lang racket
;
; Chapter 8 - Lambda the Ultimate
; Contents:
;

; rember-f test? a l: removes a from l, utilizing test? function of choice
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? (car l) a) (cdr l))
        (else (cons (car l)
                    ((rember-f test?) a
                              (cdr l))))))))
;example
((rember-f eq?) 2 '(1 2 3 4 5))

