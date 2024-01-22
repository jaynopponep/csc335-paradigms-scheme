#lang racket
;
; Chapter 8 - Lambda the Ultimate
; Contents: transforming functions, atom-to-function, the Ninth & Tenth Commandment
;
; REDEFINING:
(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))
(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))
(define (^ n m)
  (cond
    ((zero? m) 1)
    (else (* n (^ n (sub1 m))))))
(define (atom? x)
    (and (not (pair? x)) (not (null? x))))
(define operator
  (lambda (aexp)
    (car aexp)))

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


; insertL-f: transformation of insertL into insertL-f test?
(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old)
         (cons new (cons old (cdr l))))
        (else (cons (car l)
                    ((insertL-f test?) new old
                                       (cdr l))))))))
; insertR-f
(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l '()))
        ((test? (car l) old)
         (cons old (cons new (cdr l))))
        (else (cons (car l)
                    ((insertR-f test?) new old
                                       (cdr l))))))))
; insert-g: insert either at the left or at the right
; first we need a couple things:
(define seqL
  (lambda (new old l)
    (cons new (cons old l))))
(define seqR
  (lambda (new old l)
    (cons old (cons new l))))
(define seqS
  (lambda (new old l)
    (cons new l)))
; insert-g can utilize seqL and seqR which the user will choose which function to use. They can choose for insertR or insertL
(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? (car l) old)
         (seq new old (cdr l)))
        (else (cons (car l)
                    ((insert-g seq) new old
                                    (cdr l))))))))
(define insertL
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

((insert-g seqS) 'apple 'orange '(orange peanut butter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Ninth Commandment:
; Abstract common patterns with a new function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x (quote +)) +)
      ((eq? x (quote *)) *)
      (else ^))))
; above function used to identify the function as an atom and then actually utilize it as an operator like below:
(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
       ((atom-to-function (operator nexp))
        (value (1st-sub-exp nexp))
        (value (2nd-sub-exp nexp)))))))
; RETURN TO 134





