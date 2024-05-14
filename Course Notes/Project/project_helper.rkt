#lang r5rs

;; helper functions for project:

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

