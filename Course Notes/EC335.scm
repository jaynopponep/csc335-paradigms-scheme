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
; Left and right parentheses will be variables lp and rp
; We will use this later for two properties 

; EXPLANATION for probability tree diagram: (EXAMPLE USED: 121232)
; The root node of the tree diagram will state that the first can either be a ( or a 1;
; however, our precondition has already stated that the precondition must be a valid
; LD-number that starts with a 1 and ends with a 2. Since the first 1 is on the far
; left, it must be a left parentheses '(', and not the numerical value 1.
; This branches to two possibilities: ( 2 and ( ).
; The branch that leads to ( ) is invalid, we do not allow empty lists, so the
; only continuing branch is ( 2.
; Node #3 {( 2} will not branch to Node #4 {( 2 (} and Node #5 {( 2 1} since these are both possibilities.
; The node Node #4 {( 2 (} will branch to Node #6 {( 2 ( 2} and also Node #7 {( 2 ( )}; however Node #7 violates empty list property.
; So far, we have the tree: Node ( -> Node #3 -> Node #4 & #5
; Node #4 -> Node #6 & Node #7 (Node #7 stops here since empty list spotted)
; Node #5 -> Node #8 & Node #9
; Node #8 will represent ( 2 1 2 while Node #9 represents ( 2 1 )
; This same pattern continues until we reach the point where there are NO MORE VALUES left to iterate through.
; Once this is the condition, since we have been counting lp and rp, we must check for imbalance
; and a valid final number (two properties that are in the next two paragraphs), and thus
; returning 'elements' representing how many valid elements there are.


; We only need to check if it identifies an empty list during the computation
; However at the final step, it uses endoflist? to check if we are at the end
; of the list.
; If so, we must check for two properties for checking for a valid LD-number:
; i) Imbalance
; Branch identifies endoflist? returns #t
; Throughout our computation, we keep track lp and rp
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

;Precondition: n is a non-negative integer:
; Postcondition: returns second left most digit of n
(define (ld2 n)
  (cond ((zero? (quotient n 100)) (modulo n 10))
        (else (ld2 (quotient n 10))))) 


; Valid LD number?
; Precondition: n is a non-negative integer.
; Postcondition: returns true if n represents a valid LD number and false otherwise

; Design Idea: All we need to do is check if n starts with and 1 and ends with a 2.
(define (valid-LD? n)
  (cond ((< n 102) #f) 
        ((not (and (= (ld n) 1) (= (rd n) 2))) #f)
        (else #t)))

; simple length function:
(define (length n)
  (cond ((< n 10) 1)
        (else (+ 1 (length (/ n 10))))))

; Readability helper functions:
; one? function that checks if an integer n is one
(define (one? n)
  (= n 1))
; two? function that checks if an integer n is two
(define (two? n)
  (= n 2))

; Make-num function to build our LD-elements:
(define (make-num p q)
  (+ q (* p (expt 10 (length q)))))
; Main function:
; An iterative function that will process all of the possible combinations of LD-elements
; given an LD-number and then count only the ones that are valid into variable 'elements'
; and return elements.
; The idea is to not exactly build the entire LD-number, but understand how many lp and rp's are used
; in each combination 

(define (possible-LD-elements n)
  (if (valid-LD? n)
      (possible-iter n (ld n) (ld2 n) (ld n) 1 0)
      0))
  
(define (possible-iter n curr next result lp rp)
  (cond 



