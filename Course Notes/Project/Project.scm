;; CSc 335
;; Spring 2024
;; Project
; #lang scheme
;(load "project_helper.scm")
; Abrar Habib - ahabib002@citymail.cuny.edu
; Jay Noppone - npornpi000@citymail.cuny.edu


;; Released April 16 2024
;; Due by 23:59:59 May 18 2024, via email 

;; I ask for complete developments and proved, working code for three problems:



(newline)
(display ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Question 1. LD-NUM;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
(newline)

;; 1.  Write and prove correct a program to input an LD number (as previously defined) which returns a list of all lists of
;; 1s and 2s which map to that number.  Use this program to check your solution to the takehome part of the midterm. 

(define (len n)
  (cond ((< n 10) 1)
  (else (+ 1 (len (/ n 10))))))

(define (rmd n)
  (modulo n 10))

(define (two? n)
  (= n 2))

(define (one? n)
  (= n 1))

(define (remove-crust n)
  (quotient (modulo n (expt 10 (- (len n) 1))) 10))

(define (slice n)
  (quotient n 10))

(define (unmake n search-1-len)
  (let ((n-length (len n)))
    (modulo n (expt 10 (- n-length search-1-len)))))

(define (make-pairs n complement)
  (let   ((n-length (len n))
          (my-comp-length (len complement)))
    (cond ((zero? complement) (get-LD-elements n))
          ((not (> my-comp-length 2)) (cons complement (get-LD-elements (modulo n (expt 10 (- n-length my-comp-length))))))
          (else (cons (search-pair-iter complement (quotient complement 100) (rmd complement) (rmd (quotient complement 100)) '())
                      (get-LD-elements (modulo n (expt 10 (- n-length my-comp-length)))))))))

(define (get-LD-elements n)
  (cond
    ((< n 100) n)
    (else 
     (search-pair-iter (remove-crust n) (quotient (remove-crust n) 100) (rmd (remove-crust n)) (rmd (quotient (remove-crust n) 100)) '()))))
(define (search-pair-iter n n-search-1 2ptr 1ptr result)
  (cond((zero? (quotient n 100)) result)
       ((zero? n-search-1) (search-pair-iter (slice n) (quotient (slice n) 100) (rmd (slice n)) (rmd (quotient (slice n) 100)) (cons (bracket-num n n-search-1) result)))
       ((not (two? 2ptr)) (search-pair-iter (slice n) (slice n-search-1) (rmd (slice n)) (rmd (slice n-search-1)) result))
       ((not (one? 1ptr)) (search-pair-iter n (slice n-search-1) 2ptr (rmd (slice n-search-1)) result))
       (else
        (search-pair-iter n (slice n-search-1) 2ptr (rmd (slice n-search-1)) (cons (bracket-num n-search-1 (unmake n (len n-search-1))) (cons (make-pairs n (slice n-search-1)) result))))))

(define (bracket-num first second)
  (cons (quotient first 10) (list (list (get-LD-elements (quotient second 10))))))

;(get-LD-elements 11112222)

(get-LD-elements 111222) ; 4
; desired results:
; (1122) (1(2)) ((12)) ((1)2)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Abrar's Code for LD NUM


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



(define (two-nyp ldnum i-left i-right)
  (cond
    ((>= i-left (length ldnum)) '())
    ((> i-right (- (length ldnum) 1)) (two-nyp ldnum (+ i-left 1) (+ i-left 2)))
    ((or (equal? (list-ref ldnum i-left) "(")
         (equal? (list-ref ldnum i-right) ")")) '())
    ((and (equal? (list-ref ldnum i-left) 1)
          (equal? (list-ref ldnum i-right) 2)
          (> (abs (- i-left i-right)) 1))
     (let ((new-ldnum (replace-ld ldnum i-left i-right)))
       (append (list new-ldnum)
               (two-nyp new-ldnum (+ i-left 1) (+ i-left 1))
               (two-nyp ldnum i-left (+ i-right 1))
               (two-nyp new-ldnum (+ i-right 1) (+ i-right 1)))))
    (else (two-nyp ldnum i-left (+ i-right 1)))))

(define (main)
  (define ldnums-list (list
                       '(1 1 2 1 2 2)
                       '(1 1 2)
                       '(1 2 3 4 5 2)
                       '(1 4 1 3 2 1 4 1 2 2)
                       '(1 1 2 2)))
  (for-each
   (lambda (ldnum)
     (let ((ldnums (two-nyp ldnum 0 1)))
       (display "Case: ") (display ldnum) (newline)
       (for-each
        (lambda (ld)
          (display ld) (newline))
        ldnums)
       (display "LDs Generated: ") (display (length ldnums)) (newline) (newline)))
   ldnums-list))

(main)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(newline)
(display ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Question 3. TLS-let*;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
(newline)
;; 3.  Write and prove correct an interpreter for TLS extended by let*.

(define (build s1 s2)
    (cons s1 (cons s2 (quote ()))))

(define first car); (1 2 3) ;returns 1
(define second cadr); (1 2 3) ;returns 2
(define third caddr); (1 2 3) ;returns 3

(define (atom? x)
    (and (not (pair? x)) (not (null? x))))

(define (sub1 x)
  (- x 1)) ; for testing our let bindings later

;; Proof
; Design Idea: The function is designed to search through a hierarchical table of environments to find the value associated with a given name.
; The table contains many different scopes, and we just search through each scope to find the correct binding. 
; If the name is not found in the current scope, the search proceeds to the next scope in the list.


; Precon:
; table is a list of enviornment scopes where each scope is a list of pairs
; name is a symbol that needs to be search within the table
; table-f is a fallback function called when the name is not found in any scope

; Postcon: Returns the value assoicated with the name if found
; Basis Step: The base case is when the table is null or empty 
; Inductive Hypothesis: Assume the function correctly finds a variable's value or calls the fallback function for a table with n environments

; Inductive Step: For each case, if the variable is found in the first scope '(car table)', 'look-in-entry' returns its value. In the case the variable is not found in the first scope, the function
; recursively calls itself with the remaining 'n' scopes '(cdr table)'
(define (lookup-in-table name table table-f)
    (if (null? table)
        (table-f name)  ; Call fallback function if the table is empty
        (lookup-in-entry name (car table) (lambda ()
                                            (lookup-in-table name (cdr table) table-f)))))

(define extend-table cons)

(define (lookup-in-entry name entry table-f)
    (if (null? entry)
        (table-f)  ; Handle empty entry
        (let ((names (names entry))
              (vals (vals entry)))
          (if (eq? (car names) name)
              (car vals)
              (table-f)))))

; Proof
; Design Idea: The function is designed to recurisvely examine each name in a list of names to check if it matches a given target name. If a match is found, it return corresponding value from a parallel
; list of values. If no match is found and it reaches the end of the list, it calls a fallback function

; Stopping Condition: The function terminations when it reaches the end of the names list (null? names) indicating that there are no more names to check



; Pre Condition:
; names = a list of symbols
; vals = a parallel list of values corresponding to each name in the names list
; entry-f = a fallback function that is called if the name is not found in the list

; Post Condition: Returns the value assoicated with the taget name if it is found in the name list

; Basis Step: The base case is when the names list is empty (null? names)

; Inductive Hypothesis: Assume that the function correctly finds a name's value or calls the fallback function when given lists of length n
; Inductive Step:
; By the inductive hypothesis, the recursive call will correctly resolve the search in the remaining lists of length n or call 'entry-f':
; If the first name in the list (car name) matches the target 'name', the function returns the corresponding value (car vals) correctly handling the match scenario.

; If there is no match, the function recurses into the rest of the list (cdr names) and (cdr vals) 
(define (lookup-in-entry-help name names vals entry-f)
    (cond ((null? names) (entry-f name))
          ((eq? (car names) name) (car vals))
          (else (lookup-in-entry-help name (cdr names) (cdr vals) entry-f))))

(define new-entry build)

(define (names entry)
  (car entry))

(define (vals entry)
  (cadr entry))

; Everything below is taken from tls-scheme, meant to help us to implement the value function. We will need to get the value of one the expressions in the let. 
(define (value e)
    (meaning e (quote ())))

(define (meaning e table)
    ((expression-to-action e) e table))

(define (expression-to-action e)
    (cond  ((atom? e) (atom-to-action e))
           (else (list-to-action e))))

(define (atom-to-action e)
  (cond ((number? e) *const)
        ((eq? e #t) *const)
        ((eq? e #f) *const)
        ((eq? e (quote cons)) *const)
        ((eq? e (quote car)) *const)
        ((eq? e (quote cdr)) *const)
        ((eq? e (quote null?)) *const)
        ((eq? e (quote eq?)) *const)
        ((eq? e (quote atom?)) *const)
        ((eq? e (quote zero?)) *const)
        ((eq? e (quote add1)) *const)
        ((eq? e (quote mul)) *const)
        ((eq? e (quote sub1)) *const)
        ((eq? e (quote number?)) *const)
        (else *identifier)))


(define (list-to-action e)
    (cond ((atom? (car e)) (cond ((eq? (car e) (quote quote)) *quote)
                                 ((eq? (car e) (quote lambda)) *lambda)
                                 ((eq? (car e) (quote cond)) *cond)
                                 ((eq? (car e) (quote let*)) *let*)
                                 (else *application)))
          (else *application)))

(define (*const e table)
  (cond ((number? e) e)
        ((eq? e #t) #t)
        ((eq? e #f) #f)
        (else (build (quote primitive) e))))

(define (*quote e table)
    (text-of e))

(define text-of second)

(define (*identifier e table)
    (lookup-in-table e table initial-table))

(define (initial-table name)
    (car (quote ())))

(define (*lambda e table)
    (build (quote non-primitive)
           (cons table (cdr e))))
  ; Proof
  ; Design Idea: The function takes a list of bindings and an environment table, and recursively process each binding by adding it to the environment. Each binding consists of a variable and its assoicated
  ; expression which is evaluated in the context of the current envirionment before being added
  
  ; Pre Condition:
  ; bindings = a list where each element is a pair `(var expr)` representing a variable and an expression to be evaluated
  ; new-table = an environment table (A list of scopes, where each scope is a list of pairs) that the function will update with new bindings
  
  ; Post Condition: Returns an updated enviornment table that includes all the bindings processed from the provided list
  ; Basis Step: The base case is when the bindings list is empty `(null? bindings)`
  ; Inductive Hypothesis: Assume that the function correctly processes a list of n bindings evaluating each expression and adding it to the environment resulting in an updated environment table
  
  ; Inductive Step: Considering the firsrt binding in the list which consists of a variable and its expression, the function evaluates val in the context of new-table to obtain its value. It then extends
  ; the new-table with this new variable-value pair using extend-table and new-entry. The function then recurisvely calls itself with the remainder of the list which contains n bindings
  
  ; Stopping Condition: The recursion stops when bindings list is empty. 
(define (*let* e table)
  (define (process-bindings bindings new-table)
    (cond ((null? bindings) new-table)
          (else (let ((binding (car bindings)))
                  (let ((var (car binding))
                        (val (meaning (cadr binding) new-table)))
                    (process-bindings (cdr bindings)
                                      (extend-table (new-entry (list var) (list val)) new-table)))))))
  (let* ((bindings (second e))        ; Extract the bindings part of the let* expression
         (body (third e))             ; Extract the body of the let* expression
         (final-table (process-bindings bindings table)))  ; Process all bindings sequentially
    (meaning body final-table)))    ; Evaluate the body in the context of the final table


(define table-of first)
(define formals-of second)
(define body-of third)

; Proof
; Design Idea: The function is designed to evaluate a list of conditional expressions within a given environment. It processes each line of a cond construct until it finds a true condition which then
; evaluates and returns the corresponding answer

; Pre Condition:
; lines = A list of pairs where each pair represent a conditional line in a cond expression. Each pair is formatted as '(question answer)'
; table = An environment table where variables are bound to variables used for expression evaluation

; Post Condition: Returns the result of evaluating the asnwer assoicated with the first true question

; Inductive Hypothesis: Assume that the function correctly evaluates a list of n conditional lines, finding the first true question and returning the corresponding answer or properly handling the absence of a
; true condition

; Inductive Step: If the else predicate applied to the question of the first line returns true, then the function evaluates and return the answer of the line '(answer-of (var lines))'. If the result of
; evaluating the question of the first line in the context of 'table' is true, the function evaluates and returns the answer of this line. If none of the conditions are met, the function recurses with the
; rest of the list which contains n lines

; Stopping Condition: The recursion stops when the 'lines' list is empty. 
(define (evcon lines table)
  (cond ((else? (question-of (car lines))) (meaning (answer-of (car lines)) table))
        ((meaning (question-of (car lines)) table) (meaning (answer-of (car lines)) table))
        (else (evcon (cdr lines) table))))

(define (else? x)
   (cond ((atom? x) (eq? x (quote else)))
         (else #f)))

(define question-of first)
(define answer-of second)

(define (*cond e table)
    (evcon (cond-lines-of e) table))

(define cond-lines-of cdr)

; Proof
; Design Idea: The function is designed to evaluate each repression in a list of expressions in the context of a given environment and to construct a new list from the result of these evaluations

; PreCondition:
; args = A list of expression that are to be evaluated
; table = An environment table where variables and functions are bound used for evaluating the expressions

; PostCondition: Returns a new list containing the results of evaluating each expression in args within the given environment table
; Base Case: The base case is when the list of expressions args is empty 
; Inductive Hypothesis (IH): Assume the function correclty evaluates a list of n expressions and constructs a list of their results

; Inductive Step (IS): The function evaluates the first expression in the list using 'meaning' in the context of the current enviornment 'table'. It then constructs a new list hwwere this result is the
; head and the tail is the result of recursively calling 'evlis' on the rest of the list

; Stopping Condition: The recursion stops when args list is completely empty 
(define (evlis args table)
    (if (null? args)
        '()  
        (cons (meaning (car args) table) (evlis (cdr args) table)))) 

(define (*application e table)
    (let ((op (car e))  
          (args (cdr e))) 
      (let ((proc (meaning op table)) 
            (argvals (evlis args table)))  
        (if (procedure? proc)
            (apply proc argvals) 
            #f))))

(define function-of car)
(define arguments-of cdr)

(define (primitive? l)
  (eq? (first l) (quote primitive)))

(define (non-primitive? l)
  (eq? (first l) (quote non-primitive)))

(define(myapply fun vals)
  (cond ((primitive? fun) (myapply-primitive (second fun) vals))
        ((non-primitive? fun) (myapply-closure (second fun) vals))))

(define (myapply-primitive name vals)
  (cond ((eq? name (quote cons)) (cons (first vals) (second vals)))
        ((eq? name (quote car)) (car (first vals)))
        ((eq? name (quote cdr)) (cdr (first vals)))
        ((eq? name (quote null?))(null? (first vals)))
        ((eq? name (quote eq?)) (eq? (first vals) (second vals)))
        ((eq? name (quote atom?)) (:atom? (first vals)))
        ((eq? name (quote zero?)) (zero? (first vals)))
        ((eq? name (quote add1)) ((lambda (x) (+ x 1)) (first vals)))
        ((eq? name (quote mul)) (* (first vals) (second vals)))
        ((eq? name (quote sub1)) (sub1 (first vals)))
        ((eq? name (quote number?)) (number? (first vals)))))

(define (:atom? x)
  (cond ((atom? x) #t)
        ((null? x) #f)
        ((eq? (car x) (quote primitive)) #t)
        ((eq? (car x) (quote non-primitive)) #t)
        (else #f)))

(define (myapply-closure closure vals)
    (meaning (body-of closure) (extend-table (new-entry (formals-of closure) vals) (table-of closure))))

; Example Usage
(value `(let* ((x 60)) x))  ; Should return 60
(value `(let* ((x 10) (y 20)) x))  ; Should return 10
(value `(let* ((x 10) (y 20)) y))  ; Should return 20
(value `(let* ((x 2) (r 3) (sd 2) (as 34)) (mul x (mul r (mul sd as))))) ; why does this return false?
(value `(let* ((x 2) (l1 (quote 1 2 3))) (mul (car l1) x)))
