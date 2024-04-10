#lang scheme
; Extra Credit for CSc33500
; Abrar Habib - ahabib002@citymail.cuny.edu
; Jay Noppone - npornpi000@citymail.cuny.edu

; Non-primitive common functions:
(define (length n)
  (cond ((< n 10) 1)
  (else (+ 1 (length (/ n 10))))))

; DESIGN IDEA:
; An LD-number given must be a valid LD-number that passes the following precondition:
; Precondition: n>=0 is an integer that starts with 1, ends with 2.
; Once we've retrieved the input, we can already count one possible LD-element,
; which is the integer itself but with the far left 1 as (, far right 2 as ).
; After this, we can focus on anything but the far left 1, and far right 2.
; So preferably, we would want to have a function that will automatically
; delete the far left and far right numbers since we do not care about these anymore
; since all of the next LD-elements will have to have the 1 and 2 as ( ) parentheses
; anyways in order to be an LD-element; these two numbers are not our worries anymore.
; Therefore, let's declare a helper function remove-crust:

; Helper function: remove-crust
; Precondition: n>=0 is an integer that passes the precondition
; Postcondition: Return n, but without the far left 1 and far right 2.
;;Note this function will require a length, which I have pre-written above since
;;this is seems to be a common-use function throughout the course.
(define (remove-crust n)
  (quotient (modulo n (expt 10 (- (length n) 1))) 10))

; Once we've removed "the crust" of the LD-number, we can start iterating from the right
; to the left. Whenever we approach a 2, we need to call a recursive function that will
; thus search for a 1 to pair with it. Note that the first step is completely skipped
; in order to avoid counting the empty list into our counter 'LD-elements' since empty
; lists are not allowed.
; This recursive call will terminate when there are no more 1s that we can pair the 2 with.
; Once this recursive call terminates, the iterative call will in turn continue looking for
; more 2s and then do exactly the same thing over again. The iterative call itself will
; 
