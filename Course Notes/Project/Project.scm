;; CSc 335
;; Spring 2024
;; Project
#lang scheme
;(load "project_helper.scm")
; Abrar Habib - ahabib002@citymail.cuny.edu
; Jay Noppone - npornpi000@citymail.cuny.edu


;; Released April 16 2024
;; Due by 23:59:59 May 18 2024, via email 

;; I ask for complete developments and proved, working code for three problems:

;; 1.  Write and prove correct a program to input an LD number (as previously defined) which returns a list of all lists of
;; 1s and 2s which map to that number.  Use this program to check your solution to the takehome part of the midterm. 

(define (length n)
  (cond ((< n 10) 1)
  (else (+ 1 (length (/ n 10))))))

(define (rmd n)
  (modulo n 10))

(define (two? n)
  (= n 2))

(define (one? n)
  (= n 1))

(define (remove-crust n)
  (quotient (modulo n (expt 10 (- (length n) 1))) 10))

(define (slice n)
  (quotient n 10))

(define (unmake n search-1-len)
  (let ((n-length (length n)))
    (modulo n (expt 10 (- n-length search-1-len)))))

(define (make-pairs n complement)
  (let   ((n-length (length n))
          (my-comp-length (length complement)))
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
        (search-pair-iter n (slice n-search-1) 2ptr (rmd (slice n-search-1)) (cons (bracket-num n-search-1 (unmake n (length n-search-1))) (cons (make-pairs n (slice n-search-1)) result))))))

(define (bracket-num first second)
  (cons (quotient first 10) (list (list (get-LD-elements (quotient second 10))))))

(get-LD-elements 111222) ; 4
; desired results:
; (1122) (1(2)) ((12)) ((1)2)
;; 2.  Write and prove correct a syntax checker for TLS-scheme, as specified in HW 11.

;; 3.  Write and prove correct an interpreter for TLS extended by let*.
