#lang racket
;
; Chapter 4 - Numbers Games
; Contents: operations, comparators, basic functions, revision, the fifth commandment.
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


; tup+: adds the first of tup1 to the first of tup2 to a new tup, then the second of tup1 to the second of tup2, then so forth.
(define (tup+ tup1 tup2)
  (cond
    ((null? tup1) tup2)
    ((null? tup2) tup1)
    ((and (null? tup1) (null? tup2))
     '())
    (else
     (cons (o+ (car tup1) (car tup2))
           (tup+
            (cdr tup1) (cdr tup2))))))
; example:
(tup+ '(2 3) '(4 6))


; > comparator
(define (> n m)
  (cond
    ((zero? n) #f)
    ((zero? m) #t)
    (else (> (sub1 n) (sub1 m)))))
; < comparator
(define (< n m)
  (cond
    ((zero? m) #f)
    ((zero? n) #t)
    (else (< (sub1 n) (sub1 m)))))
; = comparator
(define (= n m)
  (cond
    ((> n m) #f)
    ((< n m) #f)
    (else #t)))
;example:
(= 1 1)
; ^ factorial operator
(define (^ n m)
  (cond
    ((zero? m) 1)
    (else (o* n (^ n (sub1 m))))))
;example:
(^ 2 10)
; / division operator
(define (/ n m)
  (cond
    ((< n m) 0)
    (else (add1 (/ (- n m) m)))))
;example:
(/ 90 2)


; length (of a lat)
(define (length lat)
  (cond
    ((null? lat) 0)
    (else (add1 (length (cdr lat))))))
;example:
(length '(a b c d e f))
; pick n lat: choose the nth item from lat. Note that lat's start with 1, not 0 like typical arrays
(define (pick n lat)
  (cond
    ((zero? (sub1 n)) (car lat))
    (else (pick (sub1 n) (cdr lat)))))
;example:
(pick 2 '(a b c d e f g))
; rempick n lat: pick and remove the nth item from lat.
(define (rempick n lat)
  (cond
    ((zero? (sub1 n)) (cdr lat))
    (else
     (cons (car lat) (rempick (sub1 n) (cdr lat))))))
;example:
(rempick 3 '(hotdogs with hot mustard))
; no-nums: in a lat, give only the non-number values built into a new lat
(define (no-nums lat)
  (cond
    ((null? lat) '())
    (else (cond
            ((number? (car lat))
             (no-nums (cdr lat)))
          (else (cons (car lat)
                      (no-nums (cdr lat))))))))
;example:
(no-nums '(5 pears 6 prunes 9 dates))
; all-nums: extracts a tup from a lat using all numbers in the lat.
(define (all-nums lat)
  (cond
    ((null? lat) '())
    (else
     (cond
       ((number? (car lat))
        (cons (car lat)
              (all-nums (cdr lat))))
       (else (all-nums (cdr lat)))))))
;example:
(all-nums '(5 pears 6 prunes 9 dates))
; eqan?:
(define (eqan? a1 a2)
  (cond
    ((and (number? a1) (number? a2))
     (= a1 a2))
    ((or (number? a1) (number? a2))
     #f)
    (else (eq? a1 a2))))
;example:
(eqan? '99 '99)
; occur a lat: checks how many times the atom a occurs in lat
(define (occur a lat)
  (cond
    ((null? lat) 0)
    (else
     (cond
       ((eq? (car lat) a)
        (add1 (occur a (cdr lat))))
       (else (occur a (cdr lat)))))))
(occur 'and '(apples and oranges and grapes))
; one?: returns #t if n is 1, #f otherwise
;(define (one? n)
;  (cond
;    ((zero? n) #f)
;    (else (zero? (sub1 n)))))
; we could also make it simpler than this:
(define (one? n)
    (= n 1))
(one? 1)
(one? 2)

; rempick REWRITE that uses one?:
(define (rempick-2 n lat)
  (cond
    ((one? n) (cdr lat))
    (else
     (cons (car lat) (rempick-2 (sub1 n) (cdr lat))))))
(rempick-2 2 '(hotdogs with hot mustard))
