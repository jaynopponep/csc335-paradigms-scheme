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

(get-LD-elements 11112222)

(get-LD-elements 111222) ; 4
; desired results:
; (1122) (1(2)) ((12)) ((1)2)


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
    (cond ((atom? expr) (valid-atom? expr))  ; Check if the atom is valid
          ((list? expr) (valid-list? expr))  ; Check if the list form is valid
          (else #f)))                      ; Anything else is invalid


; Helper function to check if an atom is valid
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
    (and (not (null? list))                ; Non-empty list
         (valid-list-form? (car list))     ; List's can be quotes, lambdas, define's etc.
         (apply valid-args? (cdr list))))  ; Valid arguments

; Check the head of a list for a valid form
(define valid-list-form?
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

; returns number of arguments the operation takes
(define (num-args op)
  (cond ((eq? op `cons) 2)
        ((eq? op `car) 1)
        ((eq? op `cdr) 1)
        ((eq? op `null?) 1)
        ((eq? op `atom?) 1)
        ((eq? op `zero?) 1)
        ((eq? op `add1) 1) ; add 1
        ((eq? op `mul) 2) ; multiply
        ((eq? op `sub1) 1) ; subtract 1
        ((eq? op `number?) 1)
        (else 0)))

(define (operator? exp)
  (> (num-args (car exp)) 0))

(define (correct-args-len op arg)
  (= (num-args op) (list-length arg)))

; ensure all arguments are valid
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

(valid-syntax? `3) ; #t
(valid-syntax? `(cons 1 2)) ; #t
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
