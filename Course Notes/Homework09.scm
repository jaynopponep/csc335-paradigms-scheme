; Ninth Homework Set
; CSc 335

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; First Problem

; replace-nth

; Here is a tree recursion somewhat more complicated than those we have looked at until now


; develop and certify a scheme program replace-nth which takes as input

;        a list lst, not necessarily a list of atoms
;        a positive integer, n
;        an atom, old
;        an atom, new

; (replace-nth lst n old new) should replace the nth occurrence of old in 
; lst by new (and leave everything else unchanged)

; say we have a lst: (a (a (a (a a a) (a a) a)))
; and new was b
; if n were 6, we want to have the new tree: (a (a (a (a a b) (a a) a)))
; we can have a count that starts at 0, and increments each time it detects an atom: old
; we should also check if the current "car" value is an atom or a list.
; if it is a list, then we should traverse through the entire list;
; this should be a recursive call to replace-nth that returns count
; and starts with the most recent count calculated
; the main thing is we must always make sure that count never exceeds n
; actually, we don't need a count variable. we can simply decrement n,
; and once n = 0 (or 1?), then we replace the old with the new, then return
; the entire list with that replacement

;helper function: trav-innerlst:
; pre: passes through current lst & current n it will return the rest of the list if it detects n = 0
(define (trav-innerlst lst n)
  (


;example call:
(define (replace-nth lst n old new)
  (cond
    ((null? (cdr lst)) n)
    ((atom? (car lst))
     (if (eq? (car lst) old) (replace-nth (cdr lst) (- n 1) old new)
         (replace-nth (cdr lst) n old new)))
    (else 
(replace-nth '(a (a (a (a a a) (a a) a))) '4 'a 'b)
     
        


; Additional Problems

; Abelson and Sussman, Exercise 2.27 
; Abelson and Sussman, Exercise 2.29
; Abelson and Sussman, Exercise 2.32
; Abelson and Sussman, Exercise 2.37
; Abelson and Sussman, Exercise 2.41
; Abelson and Sussman, Exercise 2.42







