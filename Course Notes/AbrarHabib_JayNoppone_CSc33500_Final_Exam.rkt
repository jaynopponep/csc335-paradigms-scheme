
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

(define first car)

(define second cadr)

(define third caddr)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

; The global environment is defined as global-env and is initially set to an empty list.
; This global environment will hold all variable bindings globally accessible throughout the interpreter's execution.

(define global-env '())

; Environment structure: a list of frames, each frame is a list of pairs (variable . value)
(define empty-env '())

; Environments are represented as lists of frames, where each frame is a list of pairs of the form:(variable . value).

(define extend-env
  (lambda (vars vals env)
    (cons (map cons vars vals) env)))

; Define an error function for reporting unbound variables
(define error
  (lambda (msg var)
    (display msg)
    (display ": ")
    (display var)
    (newline)
    ))

; The lookup function searches for a variable in the given environment.
; If the variable is not found, it reports an error.
(define lookup
  (lambda (var env)
    (cond
      ((null? env) (error "Unbound variable" var))
      (else
        (let ((frame (car env)))
          (let ((binding (assoc var frame)))
            (if binding
                (cdr binding)
                (lookup var (cdr env)))))))))

; The value function acts as the top-level function for evaluating expressions.
; It calls meaning with the expression and the global environment.
(define value
  (lambda (e)
    (meaning e global-env)))

; meaning dispatches the expression to the appropriate handler based on its type (constant, identifier, application, etc.).
(define meaning
  (lambda (e env)
    ((expression-to-action e) e env)))

; taken directly from tls-scheme.scm
(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
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
      ((atom? (car e))
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
      ((eq? (car (car lines)) 'else) (meaning (second (car lines)) env))
      ((meaning (car (car lines)) env) (meaning (second (car lines)) env))
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
    (myapply
     (meaning (function-of e) env)
     (evlis (arguments-of e) env))))

(define function-of car)

(define arguments-of cdr)

(define myapply
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (myapply-primitive (second fun) vals))
      ((non-primitive? fun)
       (myapply-closure (second fun) vals)))))

(define myapply-primitive
  (lambda (name vals)
    (cond
      ((eq? name 'cons) (cons (first vals) (second vals)))
      ((eq? name 'car) (car (first vals)))
      ((eq? name 'cdr) (cdr (first vals)))
      ((eq? name 'null?) (null? (first vals)))
      ((eq? name 'eq?) (eq? (first vals) (second vals)))
      ((eq? name 'atom?) (atom? (first vals)))
      ((eq? name 'zero?) (zero? (first vals)))
      ((eq? name 'add1) (+ (first vals) 1))
      ((eq? name 'sub1) (- (first vals) 1))
      ((eq? name 'number?) (number? (first vals)))
      ((eq? name 'mul) (* (first vals) (second vals)))
      (else (error "Unsupported primitive operation" name)))))

(define myapply-closure
  (lambda (closure vals)
    (let ((closure-env (first closure))
          (formals (second closure))
          (body (third closure)))
      (meaning body (extend-env formals vals closure-env)))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))


  

; Environment handling functions
(define extend-env
  (lambda (vars vals env)
    (cons (map cons vars vals) env)))

(define lookup
  (lambda (var env)
    (let loop ((env env))
      (cond
        ((null? env) (error "Unbound variable" var))
        (else
          (let ((frame (car env)))
            (let ((binding (assoc var frame)))
              (if binding
                  (cdr binding)
                  (loop (cdr env))))))))))

; Lexical scoping test functions
(define lexical-printer
  (lambda (env)
    (display "in lexical-printer, n=")
    (display (lookup 'n env))
    (newline)))

(define lexical-test-func
  (lambda (n outer-env)
    (let ((local-env (extend-env '(n) (list n) outer-env)))
      (display "in lexical-test-func, n=")
      (display (lookup 'n local-env))
      (newline)
      (lexical-printer outer-env)))) ; Call lexical-printer with the outer environment

; Dynamic scoping test functions
(define dynamic-printer
  (lambda (env)
    (display "in dynamic-printer, n=")
    (display (lookup 'n env))
    (newline)))

(define dynamic-test-func
  (lambda (n dynamic-global-env)
    (let ((local-env (extend-env '(n) (list n) dynamic-global-env)))
      (display "in dynamic-test-func, n=")
      (display (lookup 'n local-env))
      (newline)
      (dynamic-printer local-env)))) ; Call dynamic-printer with the local environment

; Run the lexical scoping test
(define lexical-test
  (lambda ()
    (let ((outer-env (extend-env '(n) '(100) '())))
      (display "Lexical Scoping Test")
      (newline)
      (display "in main program, n=")
      (display (lookup 'n outer-env))
      (newline)
      (lexical-test-func 1 outer-env)
      (lexical-printer outer-env))))

; Run the dynamic scoping test
(define dynamic-test
  (lambda ()
    (let ((dynamic-global-env (extend-env '(n) '(100) '())))
      (display "Dynamic Scoping Test")
      (newline)
      (display "in main program, n=")
      (display (lookup 'n dynamic-global-env))
      (newline)
      (dynamic-test-func 1 dynamic-global-env)
      (dynamic-printer dynamic-global-env))))

; Execute both tests
(lexical-test)
(dynamic-test)