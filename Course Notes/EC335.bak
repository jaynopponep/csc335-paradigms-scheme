#lang scheme
; Extra Credit for CSc33500
; Abrar Habib - ahabib002@citymail.cuny.edu
; Jay Noppone - npornpi000@citymail.cuny.edu

; Design Idea: If we graph all of the possible combinations of LD-numbers,
; in the process, we must stop whenever we reach a combination that includes
; any 12 becoming (), because this is the empty list so it is not possible
; for this to be an LD-number.

; Precondition of our iterative function:
; Valid LD-number MUST start with a 1 and ends in a 2.
; Postcondition:
; Number of elements/combinations of LD represented by a given LD-number

; We would need a helper function "endoflist?" that will check if we have reached
; the end of the list:

; Helper Function: endoflist?
; Precondition: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Postcondition: Returns #t if we are at the end of the list
; Returns #f if we are not at the end of the list

; To create a development for this program, we should create a tree similar
; to a probability tree that each branch represents different possible outcomes 
; During the computation of combinations possible within this tree,
; if it identifies a formation of an empty list (), then it must stop
; that branch immediately. However other branches will continue
; forming their own element of an LD of the LD-number
; Throughout the computation, we must also keep track of how many
; left and right parentheses we are accepting in the branch.
; We will use this later for two properties 


; We only need to check if it identifies an empty list during the computation
; However at the final step, it uses endoflist? to check if we are at the end
; of the list.
; If so, we must check for two properties for checking for a valid LD-number:
; i) Imbalance
; Branch identifies endoflist? returns #t
; Throughout our computation, we keep track of left and right parentheses,
; say variables lp and rp.
; If lp != rp, then the created LD element is unbalanced;
; therefore not a valid LD element
; If lp == rp, then the created LD element is balanced;
; However, we must check for the next property to verify
; that it is a completely VALID LD element

; ii) Valid Final Number
; Here, we simply check if the final number, 2, is labeled as a right parentheses
; or the number 2 itself.
; If it is a right parentheses: we declare it is a valid LD element, and we
; increment a variable "elements" by 1 (+ 1 elements)
; Otherwise, if it is the number 2 itself, we simply return 0 since it's not
; a valid LD element. 

; Helper Functions:
; Precondition: n is a non-negative integer
; Post-condition: returns the leftmost digit of n
(define (ld n)
  (cond ((zero? (quotient n 10)) n)
        (else (ld (quotient n 10)))))


; Precondition: n is a non-negative integer
; Postcondition: returns the rightmost digit of n
(define (rd n) (modulo n 10))


; Valid LD number?
; Precondition: n is a non-negative integer.
; Postcondition: returns true if n represents a valid LD number and false otherwise

; Design Idea: All we need to do is check if n starts with an 1 and ends with a 2.
(define (valid-LD? n)
  (cond ((< n 102) #f) 
        ((not (and (= (ld n) 1) (= (rd 2)))) #f)
        (else #t)))

; 
; 181121322156122
; (8 (1 2 (3))(5 6 1))
; (8112132215612)

; 121232 -> can only be (2123) 
