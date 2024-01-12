#lang racket
;
; Chapter 3 - Cons the Magnificent
; Contents: rember, firsts, insertR/L, subst & subst2, multirember, multiinsertL/R, 
; multisubst, the second, third, and fourth commandment

; rember a lat: "rember" stands for remove a member, removes the first occurrence of member 'a' from a lat.
; We must define rember because it is not primitive:
(define (rember a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (rember a (cdr lat))))))
; Breakdown:
; - First line of condition checks if lat is the null list. If so return the null list (Base case)
; - Second: Ask if car lat is equal to atom a. If so, then just return everything else because we found our member that is in the lat.
; Instead of removing anything, we simply utilize cdr to return anything but the first element of the list
; - Third: With cons, we can add car lat to the rember of the rest of the lat.
; This prevents the loss of information and deleting everything that we evaluated as not eq to a
; Example:
(rember 'mint '(lamb chops and mint flavored mint jelly))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Second Commandment: Use cons to build lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; firsts: Function takes an argument, a list, that is either the null list or contains S-expressions
; The function then creates a list of the first S-expression of each internal list given
(define (firsts l)
  (cond
    ((null? l) '())
    (else (cons (car (car l))
                (firsts (cdr l))))))
; example:
(firsts '((a b) (c d) (e f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Third Commandment: When building a list, describe the first typical element, and then cons it onto the natural recursion.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Note that (car (car l)) is the typical first element referred to in the Third Commandment
; The natural recursion is stored within (firsts (cdr l)) since it is simply the function calling itself with a different argument


; insertR new old lat: this function insertR (insertRight) takes three arguments: new, old, lat
; The function creates a new lat where new is inserted to the right of the first occurrence of old
(define (insertR new old lat)
  (cond
    ((null? lat) '())
    (else
     (cond
       ((eq? (car lat) old) (cons old (cons new (cdr lat))))
       (else (cons (car lat)
                   (insertR new old
                            (cdr lat))))))))
; example:
(insertR 'topping 'fudge '(ice cream with fudge for dessert))
    

; insertL new old lat: insertLeft, inserts the new atom to the left of the first occurrence of old
(define (insertL new old lat)
  (cond
    ((null? lat) '())
    (else
     (cond
       ((eq? (car lat) old) (cons new lat))
       (else (cons (car lat)
                   (insertL new old
                            (cdr lat))))))))
; example:
(insertL 'topping 'fudge '(ice cream with fudge for dessert))


; subst new old lat: substitute the first occurrence of the old atom with the new atom within the lat. 
(define (subst new old lat)
  (cond
    ((null? lat) '())
    (else (cond
            ((eq? (car lat) old)
             (cons new (cdr lat)))
            (else (cons (car lat)
                        (subst new old
                               (cdr lat))))))))
; example:
(subst 'topping 'fudge '(ice cream with fudge for dessert))


; subst2 new o1 o2 lat: substitutes the value new into either o1 or o2, whichever appears first. 
(define (subst2 new o1 o2 lat)
  (cond
    ((null? lat) '())
    (else (cond
            ((or (eq? (car lat) o1) (eq? (car lat) o2))
             (cons new (cdr lat)))
            (else (cons (car lat)
                        (subst2 new o1 o2
                                (cdr lat))))))))
; example:
(subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))


; multirember a lat: removes all occurences of the member a found in lat
(define (multirember a lat)
  (cond
    ((null? lat) '())
    (else
     (cond
       ((eq? (car lat) a) (multirember a (cdr lat)))
       (else (cons (car lat)
                   (multirember a (cdr lat))))))))
; example:
(multirember 'cup '(coffee cup tea cup and hick cup))
; breakdown of function:
; multirember a lat is very similar to the other functions; however, after checking if the first value is the same as the member we
; are finding, we recursively call the function again with a (cdr lat) as the argument, basically ignoring the member and calling the rest of
; the function and never adding the atom to the list we are constructing with cons.


(define (multiinsertR new old lat)
  (cond
    ((null? lat) '())
    (else
     (cond
       ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
       (else (cons (car lat) (multiinsertR new old (cdr lat))))))))
; example:
(multiinsertR 'topping 'with '(ice with cream with fudge with for dessert))


(define (multiinsertL new old lat)
  (cond
    ((null? lat) '())
    (else
     (cond
       ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
       (else (cons (car lat) (multiinsertL new old (cdr lat))))))))
; example:
(multiinsertL 'topping 'with '(ice with cream with fudge with for dessert))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Fourth Commandment: Always change at least one argument while recurring.
; It must be changed to be closer to termination.
; The changing argument must be tested in the termination condition: when using cdr, test termination with null?.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (multisubst new old lat)
  (cond
    ((null? lat) '())
    (else
     (cond
       ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
       (else (cons (car lat) (multisubst new old (cdr lat))))))))
; example:
(multisubst 'topping 'with '(ice with cream with fudge with for dessert))


