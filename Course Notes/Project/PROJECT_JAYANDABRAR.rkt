;; CSc 335
;; Spring 2024
;; Project
; Abrar Habib - ahabib002@citymail.cuny.edu
; Jay Noppone - npornpi000@citymail.cuny.edu

;; Released April 16 2024
;; Due by 23:59:59 May 18 2024, via email 

;; I ask for complete developments and proved, working code for three problems

(newline)
(display ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Question 1. LD-NUM;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
(newline)

;; 1.  Write and prove correct a program to input an LD number (as previously defined) which returns a list of all lists of
;; 1s and 2s which map to that number.  Use this program to check your solution to the takehome part of the midterm. 

; precondition:
;     ldnum is a list consisting of the ld number. So if our ld number we want to evalute is 1122, the value of ldnum will be '(1 1 2 2).
;     i-left is an index of sorts. this is the index at which the value should be replaced/interpreted as a "(".
;     i-right is another index. this is the index at which the value should be replace/interpreted as a ")".

; postcondition:
;
; returns a new list where
;     the element at index i-left in the original ldnum list is replaced with "(".
;     the element at inddex i-right in the original ldnum list is replaced with ")".
; all other elements are left unchaged.

; algorithm description
;     it uses the map-indexed helper function to iterate over the ldnum list and apply that function to each element along with it's index.
;     if the current index i is equal to i-left, it replaces the element at that index with "(".
;     if the current index i is equal to i-right, it replaces the element at that index with ")".
;     Otherwise, it leaves the element unchanged.
;     The result is a new list with the specified replacements, which is then returned by the function.

; Example:
;    ldnum: '(1 1 2 1 2 2)
;    i-left: 0
;    i-right: 2

; the replace-ld function will go to index 0 and replace that with a "(". it will go to the index 2 and replace that number with a ")".
; the output would be:
;    '( "(" 1 ")" 1 2 2)


(define (replace-ld ldnum i-left i-right)
  (map-indexed (lambda (i x)
                 (cond ((= i i-left) "(")
                       ((= i i-right) ")")
                       (else x)))
               ldnum))

(define (map-indexed f lst)
  (let loop ((i 0) (lst lst))
    (if (null? lst)
        '()
        (cons (f i (car lst)) (loop (+ i 1) (cdr lst))))))


; precondition:
;    ldnum:   a list of elements which can include integers or parentheses (as strings). This list represents the original sequence to be processed.
;    i-left:  an integer index in the list ldnum indicating the current starting position for checking.
;    i-right: an integer index in the list ldnum indicating the current ending position for checking.

; postconditions:
;    the function returns a list of lists, where each list represents a new sequence with valid pairs of 1 and 2 replaced by "(" and ")", respectively.
;    each generated sequence is added to the output list if a valid 1-2 pair is found and processed.
;    the function processes all possible valid pairs and their recursive sub-pairs within the input list.

; Algo design:
;   Base Cases:
;      If i-left is greater than or equal to the length of ldnum, the recursion terminates and returns an empty list because it has reached the end of the list.
;      If i-right is greater than the last index of ldnum, the function recursively calls itself with i-left incremented by 1 and i-right
;        set to i-left + 2 to check the next possible pair.

;   Condition checks:
;      If the element at i-left is "(" or the element at i-right is ")", the function returns an empty list since these positions are already processed.


;   Finding Valid Pairs:

;      If the element at i-left is 1 and the element at i-right is 2, and the distance between i-left
;      and i-right is more than 1 (to ensure there is at least one element between them), it processes this pair:

;        Creates a new list new-ldnum with the elements at i-left and i-right replaced by "(" and ")", respectively, using the replace-ld function.

;        Recursively calls two-nyp on three cases:
;            To find pairs within the first NYP (nested pairs) starting from i-left + 1.
;            To continue searching for pairs ignoring the current pair by moving i-right to the next position.
;            To find pairs within the second NYP starting from i-right + 1.


;   Combining Results:
;      Combines the results of all recursive calls using append and returns the combined list.


; Example:
;    ldnum: (1 1 2 1 2 2)
;    i-left: 0
;    i-right: 1

; The function will:
;    Identify the pair (1 2) at positions 0 and 2.
;    Replace these with "(" and ")" resulting in ("(" 1 ")" 1 2 2).
;    Recursively process the resulting list and other sublists.



(define (generate-ld ldnum i-left i-right)
  (cond
    ((>= i-left (length ldnum)) '())
    ((> i-right (- (length ldnum) 1)) (generate-ld ldnum (+ i-left 1) (+ i-left 2)))
    ((or (equal? (list-ref ldnum i-left) "(")
         (equal? (list-ref ldnum i-right) ")")) '())
    ((and (equal? (list-ref ldnum i-left) 1)
          (equal? (list-ref ldnum i-right) 2)
          (> (abs (- i-left i-right)) 1))
     (let ((new-ldnum (replace-ld ldnum i-left i-right)))
       (append (list new-ldnum)
               (generate-ld new-ldnum (+ i-left 1) (+ i-left 1))
               (generate-ld ldnum i-left (+ i-right 1))
               (generate-ld new-ldnum (+ i-right 1) (+ i-right 1)))))
    (else (generate-ld ldnum i-left (+ i-right 1)))))


; This main function was written to
(define (main)
  (define ldnums-list (list
                       '(1 1 2 1 2 2)
                       ; the ones below are all test cases.
                       ;'(1 1 2)
                       ;'(1 2 3 4 5 2)
                       ;'(1 4 1 3 2 1 4 1 2 2)
                       ; '(1 1 2 2)
                       ))
  (for-each
   (lambda (ldnum)
     (let ((ldnums (generate-ld ldnum 0 1)))
       (display "Case: ") (display ldnum) (newline)
       (for-each
        (lambda (ld)
          (display ld) (newline))
        ldnums)
       (display "LDs Generated: ") (display (length ldnums)) (newline) (newline)))
   ldnums-list))

 ;(main)

(define (testld ldnum)
  ; get the list of all possible ldnums generated
  (let ((ldnums (generate-ld ldnum 0 1)))
    ; display the current ld num being evaluated
    (display "Case: ") (display ldnum) (newline)
    (define (display-results results)
      ; if there are no ldnums generated, return an empty list
      (if (null? results)
          '()
          ; this ensures all the following expressions are evaluted in order from top to bottom. we don't want printing to be evaluated weirdly.
          (begin
            ; display the first ldnum generated and a newline
            (display (car results)) (newline)
            ; iteratively display the rest of the items. we are essentially cdr'ing down the list
            (display-results (cdr results)))))
    (display-results ldnums)
    (display "LDs Generated: ") (display (length ldnums)) (newline) (newline)))

;; Test cases
(testld '(1 1 2 1 2 2))
(testld '(1 1 2))
(testld '(1 2 3 4 5 2))
(testld '(1 4 1 3 2 1 4 1 2 2))
(testld '(1 1 2 2))


(newline)
(display ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Question 2- tls syntax checker;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
(newline)

;; 2.  Write and prove correct a syntax checker for TLS-scheme, as specified in HW 11.
(define (atom? x)
    (and (not (pair? x)) (not (null? x))))

(define (operator expr)
  (car expr))

(define (args expr)
  (cdr expr))

(define (list-length expr)
  (cond ((null? expr) 0)
        (else (+ 1 (list-length (cdr expr))))))


(define (self-eval? exp)
  (and (atom? exp) (or (number? exp) (boolean? exp))))


; Helper function to check if the expression is syntactically valid
(define (valid-expr? expr)
    (cond ((atom? expr) (valid-atom? expr))  ; If the expression is an atom, check for validity.
          ((list? expr) (valid-list? expr))  ; If the expression is a list, check for list-validity.
          (else #f)))                        ; Anything else that comes up is invalid.


; Helper function to check if an atom is valid
; In tls-scheme, the only valid atoms are cons, car, cdr, null?, eq?, atom?, zero?, add1, mul, sub1, number?. We check for those here.

(define (valid-atom? atom)
    (or (number? atom)
        (boolean? atom)
        (eq? atom (quote cons))
        (eq? atom (quote car))
        (eq? atom (quote cdr))
        (eq? atom (quote null?))
        (eq? atom (quote eq?))
        (eq? atom (quote atom?))
        (eq? atom (quote zero?))
        (eq? atom (quote add1))
        (eq? atom (quote mul))
        (eq? atom (quote sub1))
        (eq? atom (quote number?))))


; Helper function to check list expressions
(define (valid-list? list)
    (and (not (null? list))                ; A valid list expression cannot be non-empty.
         (valid-list-type? (car list))     ; List's can be quotes, lambdas, define's etc. We check for those types here.
         (apply valid-args? (cdr list))))  ; Otherwise, we check to see if the rest of the expression has valid arguments. 

; There are different types/forms of lists. It could be a quote, lambda, cond, or an atom + valid list expr.
(define valid-list-type?
  (lambda (form)
    (or (eq? form (quote quote))
        (eq? form (quote lambda))
        (eq? form (quote cond))
        (valid-application? form))))       ; Checks for built-in functions

; Function to check if function application is valid
(define valid-application?
  (lambda (fun)
    (or (valid-atom? fun)                  ; Must be a valid atom or
        (and (list? fun) (valid-list? fun))))) ; another valid list expression

; returns number of arguments an operation takes. function's kind of like a lookup table.
(define (num-args op)
  (cond ((eq? op `cons) 2)
        ((eq? op `car) 1)
        ((eq? op `cdr) 1)
        ((eq? op `null?) 1)
        ((eq? op `atom?) 1)
        ((eq? op `zero?) 1)
        ((eq? op `add1) 1) ; add 1 as defined in tls-scheme
        ((eq? op `mul) 2) ; multiply as defined in tls-scheme
        ((eq? op `sub1) 1) ; subtract 1 as defined in tls-scheme
        ((eq? op `number?) 1)
        (else 0)))

; Helper function to check if the first item of the expression is an operator. Operators always take as input 1 or more arguments.
(define (operator? exp)
  (> (num-args (car exp)) 0))

; Helper function to check if an operator and it's arguments are valid. Use the lookup table, and check if the len of the arguments match up with the expected len.
(define (correct-args-len op arg)
  (= (num-args op) (list-length arg)))

; Helper function to check if arguments are valid. The expression itself must be a valid expression AND the arguments must be of the correct length.
(define (valid-args? op args)
    (and (valid-expr? op) (correct-args-len op args)))

; valid-syntax function. A valid syntax consists of no symbols. If the expression is am atom, we need to check if it's valid and self evaluating.
; Otherwise, we need to check if the operator and the arg's being passed to it are valid. 
(define (valid-syntax? exp)
  (cond ((symbol? exp) #f)
        ((atom? exp) (and (valid-atom? exp) (self-eval? exp)))
        (else (valid-args? (operator exp) (args exp)))))

(display ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;True Conditions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
(newline)

(valid-syntax? `3) ; #t. this is just an atom, which should be valid
(valid-syntax? `(cons 1 2)) ; #t. This is a valid expression type/form and the number of arguments are correct.
; The rest of these are all basically checking if our valid-syntax function recognizes the built in functions/special forms and their arg count.
(valid-syntax? `(number? 1))   ; #t
(valid-syntax? `(add1 2)) ; #t
(valid-syntax? `(sub1 2)) ; #t
(valid-syntax? `(mul 2 2)) ; #t
(valid-syntax? `(sub1 (sub1 2))) ; #t
(valid-syntax? `(mul (mul (mul 2 3) 3) 4)) ; #t
(valid-syntax? `(atom? `cons)) ; #t
(valid-syntax? `(car `(1 2))) ; #t
(valid-syntax? `(cons 1 2)) ; #t
(valid-syntax? `(cons 1 `())) ; #t

(newline)
(display ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;False Conditions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
(newline)

               
; Checking arg's for functions, in this case cons.
(valid-syntax? `(cons 1 2 3)) ; #f
(valid-syntax? `(cons)) ; #f
(valid-syntax? `cons) ; #f

; Scheme does not allow for algebraic functions
(valid-syntax? `(+ 3 3)) ; #f 
(valid-syntax? `(3 + 3)) ; #f

; Unbound checking. "test" does not exist in this scope, and is therefore unbound. 
(valid-syntax? `test) ; #f
(valid-syntax? `(test 420)) ; #f
(valid-syntax? `(test (test2 test3))) ; #f