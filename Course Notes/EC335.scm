#lang scheme
; Extra Credit for CSc33500
; Abrar Habib - ahabib002@citymail.cuny.edu
; Jay Noppone - npornpi000@citymail.cuny.edu

; Non-primitive common functions:
(define (length n)
  (cond ((< n 10) 1)
  (else (+ 1 (length (/ n 10))))))

(define (rmd n)
  (modulo n 10))

(define (two? n)
  (= n 2))
(define (one? n)
  (= n 1))

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
; Let's say our recursive function constantly returns a smaller and smaller list,
; removing the right digit by using quotient. Once it reaches the condition that
; modulo n 10 is a 1, then we must somehow add 1 for now to the total LD-elements,
; and also call the ITERATIVE function with the parameter (quotient n 10) while (modulo n 10) is 1.
; In our example, we have 121212 1212, so our parameter here would be 121212.
; Say that our iterative function is called search-2, we would basically call (search-2 121212)
; and then expect the iterative function to return the total LD-elements found within that function.
; For context, the now LD-number 121212 should have three LD-elements:
; (2121), (21)12, 12(21). So therefore, (search-2 121212) should return 3, and then we add
; 1 to the call itself because at this point, we have just discovered that there is an LD-element
; 1 2 1 2 1 2 ( 2 1 ), that exists, which is NOT the same as ( 2 1 2 1 ) ( 2 1 ) however is simply;
; a branch of multiple LD-elements that include the ending of ( 2 1 ).
; To be clear, this means our total so far is 4, or (+ 1 (search-2 121212)) which is (+ 1 3).

; HOW DO WE HANDLE DUPLICATES?
; We actually do NOT need to worry about duplicates. For example, let's refer to the original
; LD-number 1212121212 (post-remove-crust)
; Eventually when we iterate to the 2nd '1' from the left, we form a LD-element: 12(212121)
; If we perform what is basically in-grouping, where we group within an already found group,
; we would call the iterative function (search-2 212121) when we discover a group to search within it.
; This then will result in 12(2(21)1) because it has found a matching pair. No other combinations are found.
; Later during our iteration, we will also encounter a discovery: 1212(21)12, which is obviously
; not the same as discovered number: 12(212121) at all.
; Theoretically, we can also consider grouping the final 2 with the 2nd from left 1
; which will become 12(2(21)1), which is exactly what we discovered before, meaning they're duplicates!
; How do we make sure we don't count this twice? We simply make sure to not perform out+out groupings,
; basically perform pairing on the outside of the recently discovered group, and ONLY stick to
; grouping WITHIN the group found.

; If there is a rule to follow for this problem, that remains equal to every single LD-number given, it would be:
; Grouping or matching pairs after the first discovery of a pair should occur EXACTLY and ONLY in the complement
; and within the discovered pair.

; Let's also specify what a "complement" also is:
; A complement is another LD-number that may potentially yield multiple LD-elements, and is located on the LEFT of the
; "first discovery" LD-element within the original LD-number. Any "complement" like list of numbers that are located
; on the RIGHT of the "first discovery" does NOT matter because this will yield duplicates. We would call this
; a "fake complement"

; Does this make sense to code?
; Refer to earlier, we discover 121212(21). We can call both (search-2 21) and (search-2 121212) at some place in
; the function overall where the first call would return 0, and second would return 3, and adding them would result
; in a total of 3 + 1 (for the discovery itself). So yes, it makes sense code-wise. We simply need to identify
; the complement (which was explained earlier) and the new group that we've created. The only thing that matters
; is basically the order of the calls, and how LD-element values are passed to each other to sum up and then return
; that final value.

; Let's start coding!

; Function: search-one
; Precondition: n>=0 is an integer
; Postcondition: Returns # of 1s that are available in the list n 

(define (search-one n)
  (cond ((zero? n) 0)
        ((not (one? (rmd n))) (search-one (quotient n 10)))
        (else ((one? (rmd n)) (+ 1 (check-complement (quotient n 10)) (search-one (quotient n 10)))))))
        
                           
; Function: check-complement
; Precondition: n>=0 is an integer
; Postcondition: If n=0 -> return 0, else return amount of LD-elements within n with the call of get-elements

(define (check-complement n)
  (cond ((zero? n) 0)
        (else (search-2-iter n (rmd n) 0))))


(define (get-elements n)
  (search-2-iter (remove-crust n) (rmd (remove-crust n)) 1))
(define (search-2-iter n rmd-of-n LD-elements)     ; n = crust-removed version of n, rmd-of-n = tracking right most digit of n, LD-elements = count of LD-elements
  (cond ((two? rmd-of-n) (search-one (quotient n 100)))  ; We use quotient n 100 to avoid all empty lists. it doesn't even matter at all what is next to the current 2.
        





