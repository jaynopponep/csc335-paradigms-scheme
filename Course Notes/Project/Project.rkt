#lang r5rs
;; CSc 335
;; Spring 2024
;; Project
;(load "project_helper.scm")
; Abrar Habib - ahabib002@citymail.cuny.edu
; Jay Noppone - npornpi000@citymail.cuny.edu


;; Released April 16 2024
;; Due by 23:59:59 May 18 2024, via email 

;; I ask for complete developments and proved, working code for three problems:

;; 1.  Write and prove correct a program to input an LD number (as previously defined) which returns a list of all lists of
;; 1s and 2s which map to that number.  Use this program to check your solution to the takehome part of the midterm. 

; (define (length n)
;   (cond ((< n 10) 1)
;   (else (+ 1 (length (/ n 10))))))

; (define (rmd n)
;   (modulo n 10))

; (define (two? n)
;   (= n 2))

; (define (one? n)
;   (= n 1))

; (define (remove-crust n)
;   (quotient (modulo n (expt 10 (- (length n) 1))) 10))

; (define (slice n)
;   (quotient n 10))

; (define (make-pairs n complement)
;   (let   ((n-length (length n))
;           (my-comp-length (length complement)))
;     (cond ((zero? complement) (get-LD-elements n))
;           ((not (> my-comp-length 2)) (get-LD-elements (modulo n (expt 10 (- n-length my-comp-length)))))
;           (else (+ (search-pair-iter complement (quotient complement 100) (rmd complement) (rmd (quotient complement 100)) 0)
;                    (get-LD-elements (modulo n (expt 10 (- n-length my-comp-length)))))))))

; (define (get-LD-elements n)
;   (search-pair-iter (remove-crust n) (quotient (remove-crust n) 100) (rmd (remove-crust n)) (rmd (quotient (remove-crust n) 100)) 1))
; (define (search-pair-iter n n-search-1 2ptr 1ptr LD-elements)
;   (cond((zero? (quotient n 100)) LD-elements)
;        ((zero? n-search-1) (search-pair-iter (slice n) (quotient (slice n) 100) (rmd (slice n)) (rmd (quotient (slice n) 100)) LD-elements))
;        ((not (two? 2ptr)) (search-pair-iter (slice n) (slice n-search-1) (rmd (slice n)) (rmd (slice n-search-1)) LD-elements))
;        ((not (one? 1ptr)) (search-pair-iter n (slice n-search-1) 2ptr (rmd (slice n-search-1)) LD-elements))
;        (else
;         (search-pair-iter n (slice n-search-1) 2ptr (rmd (slice n-search-1)) (+ LD-elements (make-pairs n (slice n-search-1)))))))

; (define (generate-LD n))

;; 2.  Write and prove correct a syntax checker for TLS-scheme, as specified in HW 11.

(define (every pred lst)
  (cond
    ((null? lst) #t) ; Base case: empty list, predicate holds for all elements
    ((pred (car lst)) (every pred (cdr lst))) ; Check predicate for current element and recurse on the rest of the list
    (else #f))) ; Predicate fails for at least one element

(define (all-symbols? lst)
  (every symbol? lst)) ; Check if all elements of the list are symbols

(define (syntax-check expr)
  (define bound-vars '()) ; List to track bound variables

  ; Helper function to add a variable to the bound-vars list
  (define (add-bound var)
    (set! bound-vars (cons var bound-vars)))

  ; Helper function to check if a variable is bound
  (define (bound? var)
    (memq var bound-vars))

  (define (check-expr e)
    (cond
      ((not (list? e)) #f) ; Must be a list
      ((null? e) #f) ; Empty list is invalid
      ((not (symbol? (car e))) #f) ; First element must be a symbol
      ((eq? (car e) 'define) ; Check for define syntax
       (and (>= (length e) 3) ; At least (define var value)
            (symbol? (cadr e))
            (add-bound (cadr e)) ; Add the variable to bound-vars
            (check-expr (caddr e)))) ; Check the body of the define
      ((eq? (car e) 'lambda) ; Check for lambda syntax
       (and (>= (length e) 3) ; At least (lambda (args) body)
            (list? (cadr e)) ; Second element must be a list of arguments
            (every symbol? (cadr e)) ; All arguments must be symbols
            (let ((args (cadr e)))
              (map add-bound args)) ; Add lambda arguments to bound-vars
            (check-expr (caddr e)))) ; Check the body of the lambda
      ((eq? (car e) 'if) ; Check for if syntax
       (and (>= (length e) 4)
            (check-expr (cadr e)) ; Check condition
            (check-expr (caddr e)) ; Check true-branch
            (check-expr (cadddr e)))) ; Check false-branch
      ((symbol? (car e)) (bound? (car e))) ; Check if symbol is bound
      (else #t))) ; If none of the above, assume valid expression

  (check-expr expr)) ; Start checking from the top-level expression

;; Test cases
(define test1 '(define x 5))
(define test2 '(lambda (x y) (+ x y)))
(define test3 '(if (> x 0) x (- x)))
(define test4 'atom?)
(define test5 '(define y 10))
(define test6 '(lambda (z) (* y z))) ; Unbound variable 'y'

(display (syntax-check test1))
(newline) ; Should return #t
(display (syntax-check test2))
(newline) ; Should return #t
(display (syntax-check test3))
(newline) ; Should return #f (variable 'x' is unbound)
(display (syntax-check test4))
(newline) ; Should return #t
(display (syntax-check test5))
(newline) ; Should return #t
(display (syntax-check test6))
(newline) ; Should return #f (variable 'y' is unbound)




; (display (syntax-check '(define x 5))) ; Should return #t
; (newline)
; (display (syntax-check '(lambda (x y) (+ x y)))) ; Should return #t
; (newline)
; (display (syntax-check '(if (> x 0) x (- x)))) ; Should return #t
; (newline)
; (display (syntax-check 'atom?)) ; Should return #f
; (newline)
; (display (syntax-check '(define))) ; Should return #f (invalid syntax for define)
; (newline)
; (display (syntax-check '(lambda x (+ x x)))) ; Should return #f (missing parentheses around arguments)
; (newline)
; (display (syntax-check '(if (> x 0)))) ; Should return #f (missing true-branch and false-branch)
; (newline)
; (display (syntax-check '(lambda (x) (+ x x)))) 





;; 3.  Write and prove correct an interpreter for TLS extended by let*.