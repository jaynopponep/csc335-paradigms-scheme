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

; ;; Note this function will require a length, which I have pre-written above since
; ;; this is seems to be a common-use function throughout the course.

(define (remove-crust n)
  (quotient (modulo n (expt 10 (- (length n) 1))) 10))

; Once we've removed "the crust" of the LD-number, we can start iterating from the right
; to the left. Whenever we approach a 2, we need to call a recursive function that will
; thus search for a 1 to pair with it. Note that the first step is completely skipped
; in order to avoid counting the empty list into a counter named 'LD-elements,' since empty
; lists are not allowed.

; This recursive call will terminate when there are no more 1s that we can pair the 2 with.
; Once this recursive call terminates, the iterative call will in turn continue looking for
; more 2s and then do exactly the same thing over again. The iterative call itself will terminate
; once there are no more elements to even iterate through.

; Specifically within the design, the iterative function will keep track of the variable
; 'LD-elements' which will increment each time we have found a pair of 1s and 2s.
; Once the recursive call that we call within the iterative function is done, we should have
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
; LD-number 1212121212 ( AFTER applying remove-crust)
; Eventually when we iterate to the second '1' from the left, we form a LD-element: 12(212121)
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

; Iterative main function:
; Design Idea: Guard calls the iterative function and the guard makes sure to remove the crust and set LD-elements to 1 since we have by default
; found a pair by itself.
; Overall, the iterative function will keep track of two pointers that both look for 2 and 1. Once both have been found, it would call
; a function create-pair that will create the pair, and be given the complement which will also find pairs within the complement.
; It will also make sure to process the main pair that we have created by looking into the pair, and calling get-LD-elements on that function again
; GI: Given that AP includes numbers 2s that are already processed: n = [NYP][AP] and n = NYP * 10^[# of digits in AP] + AP
; We are not assuming the 1s are already processed because they can be used multiple times with other 2s. However, the main iterative function
; is completely done with any 2s that it encounters after it is done pairing with it.
; We terminate the iterative function when n is a 2 digit number. Even if it is a 1 and 2, this would create an empty list, which we do not count,
; so it'll be completely unnecessary to even iterate through the final 2 digit numbers. So once ((zero? (quotient n 100)) is true, then exit the program
; and return LD-elements
; Is the GI:
; Strong enough? We spoke about the termination idea just now, which can be used in the strong enough test. If we have our original number post-crusting:
; 12, we would in total have LD-elements = 1 for the crusting, and the program will see (quotient 12 100) = 0, so NYP = 0 at this point.
; Weak enough? If we have a number 1122 which passes the precond, we would initialize LD-elements to 1 when we pass it through to the iterative function.
; New n becomes 12 and LD-element = 1. 
; Preserved? Given a number 1122 post-crusting, it is entirely NYP by default. Once it finds the first 2 and then the 1, it is not done processing until reaching
; the final 1. Once this has been done, 112 is NYP, and the far right 2 is AP. We will never revisit this specific 2 to check if it is a 2.
; So let's check: n = 112 * 10^[1] + 2 = 112 * 10 + 2 = 1120 + 2 = 1122. 
(define (get-LD-elements n)
  (search-pair-iter (remove-crust n) (quotient (remove-crust n) 100) (rmd (remove-crust n)) (rmd (quotient (remove-crust n) 100)) 1))
(define (search-pair-iter n n-search-1 2ptr 1ptr LD-elements)
  (cond ((zero? (quotient n 100)) LD-elements)
        ((not (two? 2ptr)) (search-pair-iter (slice n) (slice n-search-1) (rmd (slice n)) (rmd (slice n-search-1)) LD-elements))
        ((not (one? 1ptr)) (search-pair-iter n (slice n-search-1) 2ptr (rmd (slice n-search-1)) LD-elements))
        (else
         (search-pair-iter (slice n) (slice n-search-1) (rmd (slice n)) (rmd (slice n-search-1)) (+ LD-elements (make-pairs n n-search-1))))))

; Helper function: make-pairs
; Design Idea: Function make-pairs will basically be a helper function that will find all the pairs within the complement, within the new paired groups
; and sum them up and return it to the search-pair-iter for accumulation.
; Precondition: n>=0 is an integer where (rmd n) is a 2 only.
; Postcondition: returns a NUMERICAL value that represents number of LD-elements existing in complement and first discovered paired group.
; IH: 
; IS:
; Base Case: 112 or 122 are the most basic inputs given. Either one will return 1 since only one pair can be created because of the fact
; that there are NO complements, and within the paired group (1) or (2), there are no LD-elements within the parentheses 
(define (make-pairs n complement)
  (let ((my-comp (slice complement))
        (n-length (length n))
        (my-comp-length (length (slice complement))))
    (cond (+ (search-pair-iter my-comp (quotient my-comp 100) (rmd my-comp) (rmd (quotient my-comp 100)) 0)
             (get-LD-elements (modulo n (expt 10 (+ 1 (- n-length my-comp-length))))))))) 

; Readability helper function: slice
; Precond: n >=0 is an integer
; Postcond: returns a smaller n without the rmd of n
(define (slice n)
  (quotient n 10))
;(get-LD-elements 11112222)
;(get-LD-elements 181121322156122) ; 37
;(get-LD-elements 10101012020202) ; 14
;(get-LD-elements 111152222) ; 20
;(get-LD-elements 11111522222) ; 70
;(get-LD-elements 1111122222) ; 50
(get-LD-elements 1212222) ; 3
;(get-LD-elements 11022) ; 2
;(get-LD-elements 11822) ; 2
;(get-LD-elements 1122) ; 1
;(get-LD-elements 1121212122) ; my personal test case, 9

; Abrar's code

;(define (count-LD-elements n))

;(define (count-pairs n count)
 ; ((cond ((zero? n) count)
  ;       ((one? n) count-pairs (quotient n 10) (+ count 1))
         

;(define (count-iter n count)
 ; (let ((rmd (modulo n 10)) (complement (quotient n 100))
  ;((cond ((zero? n) count)
   ;      ((two? n) (count-pairs complement) 
