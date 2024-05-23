;; Second Problem

;; TLS is an environment passing interpreter, as we have discussed.

;; For this problem, I ask you to eliminate all environment
;; parameters, and redesign the interpreter to work with a
;; single, global, environment -- call it global-env.

;; You will find that a few modifications in addition to the
;; elimination of the environment parameters from procedures
;; will be needed to make the new system work.  One of these will --
;; holy set-bang Batman! -- require assignment.

;; When you finish, the new interpreter should work with
;; the unmodified TLS language: I am not allowing (or asking for) set!
;; at the top level.

;; Find at least one example for which old TLS and the new TLS
;; return different results.  Describe the scoping discipline
;; implemented by your new system.

;; And, of course, your code needs to be accompanied by
;; (a perhaps abbreviated) correctness proof, by structural
;; induction, once you have a description of the new scoping
;; discipline.

;; For this problem, I do NOT need pdf: just an executable
;; .scm document, with your comments / proofs embedded
;; in the usual way.

;; (You all know about Batman, right?)

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote ())))))

(define get-first car)

(define get-second cadr)

(define get-third caddr)

(define is-atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define is-primitive?
  (lambda (l)
    (eq? (get-first l) (quote primitive))))

(define is-non-primitive?
  (lambda (l)
    (eq? (get-first l) 'non-primitive)))

; The global environment is defined as global-env and is initially set to an empty list.
; This global environment will hold all variable bindings globally accessible throughout the interpreter's execution.

(define global-env '())

; Environments are represented as lists of frames, where each frame is a list of pairs of the form:(variable . value).
(define empty-env '())

(define extend-env
  (lambda (vars vals env)
    (cons (map cons vars vals) env)))

; Define an error function for reporting any error message and any variable associated with said error.
(define error
  (lambda (message var)
    (display message)
    (display ": ")
    (display var)
    (newline)))

; Curry the error function to create an error-unbound function.
(define error-unbound
  (lambda (var)
    (error "Unbound variable" var)))

; The lookup function searches for a variable in the given environment.
; If the variable is not found, it reports an error.
(define lookup
  (lambda (var env)
    (let loop ((env env))
      (cond
        ((null? env) (error-unbound var))
        (else
          (let ((frame (car env)))
            (let ((binding (assoc var frame)))
              (if binding
                  (cdr binding)
                  (loop (cdr env))))))))))

; The value function acts as the top-level function for evaluating expressions.
; It calls meaning with the expression and the global environment.
(define value
  (lambda (e)
    (meaning e global-env)))

; meaning dispatches the expression to the appropriate handler based on its type (constant, identifier, application, etc.).
(define meaning
  (lambda (e env)
    ((expression-to-action e) e env)))

(define expression-to-action
  (lambda (e)
    (cond
      ((is-atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
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
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((is-atom? (car e))
       (cond
         ((eq? (car e) (quote quote)) *quote)
         ((eq? (car e) (quote lambda)) *lambda)
         ((eq? (car e) (quote cond)) *cond)
         (else *application)))
      (else *application))))

; The functions below are different action functions.
; They define how various types of expression are evaluated. 
(define *const
  (lambda (e env)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))

(define *quote
  (lambda (e env)
    (text-of e)))

(define *identifier
  (lambda (e env)
    (lookup e env)))

(define *lambda
  (lambda (e env)
    (build (quote non-primitive)
           (cons env (cdr e)))))

(define *cond
  (lambda (e env)
    (evcon (cond-lines-of e) env)))

(define cond-lines-of cdr)

(define evcon
  (lambda (lines env)
    (cond
      ((eq? (car (car lines)) 'else) (meaning (get-second (car lines)) env))
      ((meaning (car (car lines)) env) (meaning (get-second (car lines)) env))
      (else (evcon (cdr lines) env)))))

(define evlis
  (lambda (args env)
    (cond
      ((null? args) '())
      (else
       (cons (meaning (car args) env)
             (evlis (cdr args) env))))))

(define *application
  (lambda (e env)
    (apply-function
     (meaning (function-of e) env)
     (evlis (arguments-of e) env))))

(define function-of car)

(define arguments-of cdr)

(define apply-function
  (lambda (fun vals)
    (cond
      ((is-primitive? fun)
       (apply-primitive (get-second fun) vals))
      ((is-non-primitive? fun)
       (apply-closure (get-second fun) vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name 'cons) (cons (get-first vals) (get-second vals)))
      ((eq? name 'car) (car (get-first vals)))
      ((eq? name 'cdr) (cdr (get-first vals)))
      ((eq? name 'null?) (null? (get-first vals)))
      ((eq? name 'eq?) (eq? (get-first vals) (get-second vals)))
      ((eq? name 'atom?) (is-atom? (get-first vals)))
      ((eq? name 'zero?) (zero? (get-first vals)))
      ((eq? name 'add1) (+ (get-first vals) 1))
      ((eq? name 'sub1) (- (get-first vals) 1))
      ((eq? name 'number?) (number? (get-first vals)))
      ((eq? name 'mul) (* (get-first vals) (get-second vals)))
      (else (error "Unsupported primitive operation" name)))))

(define apply-closure
  (lambda (closure vals)
    (let ((closure-env (get-first closure))
          (formals (get-second closure))
          (body (get-third closure)))
      (meaning body (extend-env formals vals closure-env)))))

; Lexical scoping test functions
(define lexical-printer
  (lambda (env)
    (display "in lexical-printer (outer environment), n=")
    (display (lookup 'n env))
    (newline)))

; The purpose of this test is to show that variables are looked up in the same environment as where their function was defined.
; Lexical scoping MEANS that variable bindings are resolved in the same environment as their
; function definition (I'm assuming this would be called compile-time in C terms), and not in the environment in which the function was ran (runtime).

; We should expect to see that "n" in the local environment is newly bound, but in the outer environment it stays the same.
(define (lexical-test-func n outer-env)
  ; create a new environment called local-env where we bind "n" the character/string to whatever value was passed in for n. we can name this bound variable whatever.
  (let ((local-env (extend-env '(n) (list n) outer-env)))
    ; display the value of n in local-env to show that it was bound properly.
    (display "in lexical-test-func (local environment), n=")
    (display (lookup 'n local-env))
    (newline)
    ; then we display the value of n with a different environment: outer-env.
    (lexical-printer outer-env)))

; Dynamic Scoping means that the scope of a variable is determined by the environment in which the function was called.
; This is more akin to runtime environment scoping (not sure exactly of the proper term to use here).
; For the dynamic scoping test, we should expect to  see the same value for n in the local environment and outer environments,
; as they are run in the same scope, but a different value for n in the global scope, as it was run in a different scope.
(define dynamic-printer
  (lambda (env)
    (display "in dynamic-printer (outer environment), n=")
    (display (lookup 'n env))
    (newline)))

(define dynamic-test-func
  (lambda (n dynamic-global-env)
    (let ((local-env (extend-env '(n) (list n) dynamic-global-env)))
      (display "in dynamic-test-func (local environment), n=")
      (display (lookup 'n local-env))
      (newline)
      (dynamic-printer local-env)))) 


(define lexical-test
  (lambda ()
    (let ((outer-env (extend-env '(n) '(100) '())))
      (display "Lexical Scoping Test")
      (newline)
      (display "in main program (outer environment), n=")
      (display (lookup 'n outer-env))
      (newline)
      (lexical-test-func 1 outer-env)
      (lexical-printer outer-env))))


(define dynamic-test
  (lambda ()
    (let ((dynamic-global-env (extend-env '(n) '(100) '())))
      (display "Dynamic Scoping Test")
      (newline)
      (display "in main program (global environment), n=")
      (display (lookup 'n dynamic-global-env))
      (newline)
      (dynamic-test-func 1 dynamic-global-env)
      (dynamic-printer dynamic-global-env))))

; Execute both tests
(lexical-test)
(dynamic-test)
