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
; more 2s and then do exactly the same thing over again. The iterative call itself will terminate
; once there are no more elements to even iterate through.

; Specifically within the design, the iterative function will keep track of the variable
; LD-elements which will increment each time we have found a pair of 1s and 2s.
; So once the recursive call that we call within the iterative function is done, we should have
; an updated value for LD-elements for each iteration. At the same time that the iterative function
; updates its LD-elements, it also updates n to be smaller, without the number "2" that we
; have now finished dealing with in the recursive function.

; This essentially suggests a very simple GI to our iterative function:
; n = [NYP][AP]
; where n is the LD-number given to us
; The AP will not be used in our function since they are the values that will not
; be needed after it has become AP; in other words, 2s that are already processed.
; NYP includes all of the leftover 2s that we must iterate to next once we are done
; with the current iteration. 
; Since we'll keep track of NYP, we terminate the iterative function when:
; = (quotient nyp 10) 0
; Note that even if we had nyp = 2, it still cannot pair with the very first 1
; that we have pre-remove-crusting since it will create an empty list. Alternatives
; would be it being non-1 AND non-2, which also doesn't matter, so we terminate
; pretty much right when nyp is a 1 digit number.
; Another GI is that n = NYP * 10^[# of digits in AP] + AP also stands as a GI
; in this iterative function. For example:
; n = 13232
; If we are at nyp = 1, n = 1 * 10^[4] + 3232 => n = 1*10000+3232 = 13232 = True

; The biggest question in this problem is, if we find a pair of 1s and 2s, doesn't that
; mean that there are multiple possible complements to that pair? For example:
; given n = 112121212122, let's remove the crust: 1212121212
; Our first pair we identify would create a complement: 121212 1212
; We see the complement 121212 can actually be multiple LD-elements itself.
; In this case, we could call the iterative function within this 121212 and this
; would basically be a separate problem within itself. We make the program do the work for us.
; However, since our LD-number can be given as any combination of digits, we are also able to get
; 12121231212, which is also the same problem since the 3 doesn't matter, but with our previous logic,
; it does not call the iterative function. This will have the complement: 1212123 and 1212.
; We must perform the iterative operation onto the complement also since it has its pairs within
; the complement, but we do not use the remove-crust function. This shows that we must use the
; remove-crust function outside of the iterative function, and use it as a guard and called before
; parameters are passed into an -iter function. We can also conclude that we must perform the
; iterative function on the given complement anyways no matter if we identify a 2 or not. 










