#lang racket
;
; Chapter 5 - It's Full of Stars
; Contents:
;
; REDEFINING:
; atom?
(define (atom? x)
    (and (not (pair? x)) (not (null? x))))
; o+
(define (o+ n m)
  (cond
    ((zero? m) n)
    (else (add1 (o+ n (sub1 m))))))


; rember* a l: remove all atoms of "a" from the list of possible lists and atoms "l"
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


; insertR* new old l: inserts new to the right of all values of old found in list l
(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l)
                     (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l))
                  (insertR* new old (cdr l)))))))
;example:
(insertR* 'topping 'fudge '(ice cream with fudge for fudge dessert))


; concept notes:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The First Commandment FINAL:
; When recurring on a list of atoms, lat, ask two questions about it: (null? lat) and else.
; When recurring on a number, n, ask two questions about it: (zero? n) and else.
; When recurring on a list of S-expressions, l, ask three questions about it: (null? l), (atom? (car l)), and else.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The functions recur on the car of its argument when the car itself is a list, basically recalling the function to get deeper into the list instead of ignoring it
; To break it down: With *functions, we are working with lists that are either: empty, atom consed onto a list or a list consed onto a list.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Fourth Commandment FINAL:
; Always change at least one argument while recurring.
; When recurring on a list of atoms, lat, use (cdr lat).
; When recurring on a number, n, use (sub1 n).
; When recurring on a list of S-expressions, l, use (car l) and (cdr l) if neither (null? l) nor (atom? (car l)) are true.
; It must be changed to be closer to termination. The changing argument must be tested in the termination condition:
; when using cdr, test termination with null? and when using sub1, test termination with zero?.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; occur* a l: checks all times where a occurs in l, including in lists.
(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (o+ (occur* a (car l))
                (occur* a (cdr l)))))))
;example:
(occur* 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))


; subst* new old l: substitutes all instances of old within l with "new"
(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new (subst* new old (cdr l))))
         (else
          (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l))
                  (subst* new old (cdr l)))))))
;example:
(subst* 'orange 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))


; insertL* new old l: insert new to the left of all instances of old in l
(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
         (else (cons (car l)
                     (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l))
                  (insertL* new old (cdr l)))))))
;example:
(insertL* 'pecker 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))


; member* a l: checks if a is a member of list l, is optimized to check all S-expressions
(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or (eq? (car l) a)
           (member* a (cdr l))))
      (else (or (member* a (car l))
                (member* a (cdr l)))))))
(member* 'chips '((potato) (chips ((with) fish) (chips))))


; leftmost l: finds the leftmost atom in a non empty list of S-expressions, which doesn't contain the empty list
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))
(leftmost '(((hot) (tuna (and))) cheese))


; eqlist? l1 l2: checks if l1 and l2 are identical/equal lists
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((and (null? l1) (atom? (car l2))) #f)
      ((null? l1) #f)
      ((and (null? l2) (atom? (car l1))) #f)
      ((and (atom? (car l1))
            (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2))))
      
