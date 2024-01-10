#lang racket
;
; Chapter 2 - Do It Again
; Contents: lats, or, member?, the first commandment
;

; Redefine atom? function:
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lat?: aka "list of atoms", finds if all S-expressions given in a list are atoms.
; First, we must define lat? because it is not a primitive.
(define (lat? x)
  (cond
    ((null? x) #t)
    ((atom? (car x)) (lat? (cdr x)))
  (else #f )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Notes of lat? function structure:
; - (cond...) asks questions; you can think of this as "conditions: ..."
; - the first condition is the question of whether argument x is a null argument. If this is so, it is automatically #t because all null lists are lats
; - the second condition asks if the first S-expression is an atom. If so, check if the rest of the list of S-expressions are atoms.
; The cdr will thus determine the result of this question. If (lat? (cdr x)) is true, then #t is returned
; - Notice that this function "lat?" calls itself by creating a new argument cdr x, which we will use the rules/conditions of this function to figure out
; - Calling the function itself again works because you would continuously check a smaller and smaller list for the first S-expression
; and at the end, there will just be a cdr x of the value () [the null list], which is by default #t.
; - Generally speaking, lat? consecutively checks each S-expression if it is a list. If it is a list, it returns #f. Otherwise, they are all atoms and it returns #t
(lat? '(bacon and eggs))     ; True because each S-expression in the list (bacon and eggs) is an atom.


; or: Asks if either the first or second question is true. If so, this function returns #t
(or (null? '(a b c)) (null? '()))    ; True because the second question answers true, since () is the null list


; member? x lat: essentially asks if a certain atom exists within a list of atoms.
; x, in context, is an atom; lat, is a list of atoms. Specifically, member? checks if x exists in lat
; Let's define member? because it is not a primitive:
(define (member? x lat)
  (cond
    ((null? lat) #f)
    (else (or (eq? (car lat) a)
              (member? a (cdr lat))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The First Commandment:
; Always ask null? as the first question in expressing any function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; REFER BACK TO P23 @SELF
