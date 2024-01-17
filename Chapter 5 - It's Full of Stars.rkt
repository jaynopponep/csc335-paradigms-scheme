#lang racket
;
; Chapter 5 - It's Full of Stars
; Contents:
;

; atom?:
(define (atom? x)
    (and (not (pair? x)) (not (null? x))))
; rember* a l: remove each atom of "a" from the list of possible lists and atoms "l"
(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (rember* a (cdr l)))
         (else (cons (car l)
                     (rember* a (cdr l))))))
      (else (cons (rember* a (car l))
                  (rember* a (cdr l)))))))
(rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))


