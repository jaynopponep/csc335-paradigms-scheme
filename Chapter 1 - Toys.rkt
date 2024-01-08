#lang racket
;
; Chapter 1 - Toys
; Contents: The Five Laws
;


; Examples of atoms:
'atom
(quote atom)   ; 'atom & (quote atom) are equivalent in Scheme.
'turkey
1492
'1492   ; Both 1492 and '1492 are atoms. However, 1492 is a number while '1492 is just an S-expression
'u
'*abc$


; Examples of lists:
'()
'(atom)
(quote (atom))
'(atom turkey or)
'((atom turkey) or)
'(how are you doing so far)   ; Note this list includes 6 S-expressions: how, are, you, doing, so, far


; Examples of non-lists:
; (atom turkey) or   ; This contains two S-expressions not enclosed by parentheses. The first is a list of two atoms, second is an atom


; Examples of S-expressions:
'xyz   ; All atoms are S-expressions
'(x y z)   ; All lists are S-expressions
'((x y) z) 


; Law of Car: The primitive car is defined only for non-empty lists. **Note car of l is interchangeable with car l
; car of l (where l is the argument (a b c)) => a   ; a is the first atom of this list
(car '(a b c))
; car of l (where l is ((a b c) x y z) => (a b c)   ; (a b c) is the first S-expression of the non-empty list
(car '((a b c) x y z))
; car of l (where l is hotdog)                      ; You cannot ask for the car of an atom. Only applicable to non-empty lists
; car of l (where l is ())                          ; () is an empty list. Car is only applicable to non-empty lists


; Law of Cdr: Defined only for non-empty lists; cdr of any non-empty list is always another list. **Note Cdr is pronounced "could-er"
; cdr l (where l is (a b c)) => (b c)            ; Cdr calls for the opposite of car
(cdr '(a b c))
; cdr of l (where l is (hamburger)) => ()
(cdr '(hamburger))
; cdr l (where l is ()) => Error                 ; You cannot ask for the cdr of the null list


; Law of Cons: The primitive cons takes two arguments. First argument is any S-expression, second must be a list, resulting in a list.
; cons of the atom a and the list l (where a is peanut and l is (butter and jelly)) => (peanut butter and jelly)
(cons 'peanut '(butter and jelly))    ; cons adds an atom or S-expression to the front of the list given
(cons '(banana and) '(peanut butter and jelly))  ; Yields ((banana and) peanut butter and jelly)


; Law of Null?: The primitive null? is defined only for lists
(null? '())    ; () is an empty list which is also the null list. True #t
(null? (quote ()))     ; This is simply the same as line 61. True #t
(null? '(a b c))     ; This is false since (a b c) is not the null list. False #f
(null? 'spaghetti)   ; You cannot ask null? of an atom. False #f


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Defining atom? for Scheme because it is not a primitive like car, cdr and null?:
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(atom? 'Harry)   ; Yields true, Harry is a string of characters, hence an atom


; Law of Eq?: The primitive eq? takes two arguments. Each must be a non-numeric atom.
(eq? (car '(beans beans we need jelly beans)) (car (cdr '(beans beans we need jelly beans))))
; Above yields true. Compares beans to beans, which are both the same atom from the list.
