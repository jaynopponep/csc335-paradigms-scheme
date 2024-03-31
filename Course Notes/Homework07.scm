#lang scheme
; Seventh Homework Set
; CSc 335

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Homework7.scm
; Here are some homework problems to get you started with lists
; Note that I have sometimes deliberately offered incomplete specifications - if you find this
; to be the case, you will need to complete the specification as you deem best.
; Give both recursive and iterative procedures (along with a development/proof) for each.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 1. Write your own version of length using the list functions we have discussed.

; The idea is suggested by (my-length '(a b c d)) = 4.
; Recursive solution
; IH: The recursive call will work since we are protected with the base case where lst is the null list
; When this is the case, we return 0 for length 0. This is also when we reach the final value in the list,
; which we add to the current sum as 0. We add +1 for each iteration through the list, thus representing the length
; When we simply cdr down the list, we are writing a new list without the car when we call (my-length (cdr list)), so
; eventually we will reach the value () as the lst when we call this. When this is the case, we return 0 to terminate
; the function and then print out the final result.
(define my-length
  (lambda (lst)
    (cond
      ((null? lst) 0)
      (else
       (+ 1 (my-length (cdr lst)))))))
(my-length '(a b c d))

; 2. Write your own version of list-ref using the list functions we have discussed.  

; Briefly, the idea is indicated by this example:  (my-list-ref '(a b c d) 2) = c.  Note the 0-based
; indexing.  What happens if the input index exceeds the size of the input list?
; IH: Recursive call will work as long as we decrease 'b' which is the index input by 1 on each call,
; and then check if b has reached 0. Once it has reached zero, we return the current car of the list.
; Pre-cond: b must not exceed the length of a
(define (my-list-ref a b)
  (cond
    ((zero? b) (car a))
    (else (my-list-ref (cdr a) (- b 1)))))
(my-list-ref '(a b c d) 2)

; 3. Write a function start that takes two arguments, lst and num, and which returns the
; first num elements of lst.
(define (first-num lst num)
  (cond ((zero? num) '())
        (else (cons (car lst) (first-num (cdr lst) (- num 1))))))
(first-num '(a b c d e) 2)
(first-num '(a b c d e) 5)

; 4.  Write a function but-last that takes two arguments, lst and num, and which returns the
; list of all but the last num elements of lst.

; Are we allowing the use of length? Or can we not use helper functions !
; If we are able to use helper functions, we would simply use the length, and then subtract (- length num),
; and then keep recursively calling the function until we reach (zero? (- length num))
(define (but-last lst num)
  (let ((len (my-length lst)))
    (cond
      ((zero? (- len num)) '())
      (else
       (cons (car lst) (but-last (cdr lst) num))))))

; 5.  Write a function end that takes two arguments, lst and num, and returns the last num
; elements of lst.

; This is practically the opposite of the but-last function, where we get all of the numbers
; except the last num numbers in a list.
; Here, we want THOSE last numbers in the list to print out.
; In the but-last function, we checked if - len num is 0, and when we did we ended the iteration and returned the null list
; Instead, we will return car AND cdr of the list at that point when (- len num) = 0. 

(define (end lst num)
  (let ((len (my-length lst)))
    (cond
      ((zero? (- len num)) lst)
      (else
       (end (cdr lst) num)))))

(end '(a b c d e f) 1)
  


; References:
; http://www.schemers.org/Documents/Standards/R5RS/



