#lang racket
;
; Chapter 7 - Friends and Relations
; Contents: set?, makeset, 
;
; REDEFINING:
; atom?
(define (atom? x)
    (and (not (pair? x)) (not (null? x))))
; member
(define (member? x lat)
  (cond
    ((null? lat) #f)
    (else (or (eq? (car lat) x)
              (member? x (cdr lat))))))
; multirember
(define (multirember a lat)
  (cond
    ((null? lat) '())
    (else
     (cond
       ((eq? (car lat) a) (multirember a (cdr lat)))
       (else (cons (car lat)
                   (multirember a (cdr lat))))))))

; set?: a list of atoms where no atom appears more than once.
; if an atom appears more than once, #f is returned
; the null lat will return true
(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))
; here we just use member? to do all the work to check if the car lat is a member of the cdr lat
; example:
(set? '(apple peaches apple plum))
(set? '(apple peaches plum))

; makeset lat: simplify a set and get rid of duplicates
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat))
       (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))
;example:
(makeset '(apple peach pear peach plum apple lemon peach))
; let's redefine makeset using multirember, which will make this more efficient:
(define makeset-new
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat)
                  (makeset-new (multirember (car lat) (cdr lat))))))))
; while adding the first term to the new set we are creating, we remove every single duplicate
; or instance from the cdr lat. We will not encounter duplicates of the car lat again.
; This simply just keeps adding each value one by one, while also removing duplicates with
; multirember.
;example:
(makeset-new '(apple 3 pear 4 9 apple 3 4))


; subset? set1 set2: checks if all atoms in set1 are also in set2
(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else
       (and (member? (car set1) set2)
            (subset? (cdr set1) set2))))))
(subset? '(6 large chickens with wings) '(6 chickens with large wings))


; eqset? set1 set2: checks if all atoms are the equivalent in both set1 and set2, no matter the order
(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
           (subset? set2 set1))))
(eqset? '(6 large chickens with wings) '(6 chickens with large wings))


; intersect? set1 set2: checks if there's atleast one atom in common between both sets
(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))
;example:
(intersect? '(stewed tomatoes and macaroni) '(mac and cheese))
; intersect set1 set2: return what intersects between both sets.
(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (cons (car set1)
             (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))
;example:
(intersect '(stewed tomatoes and macaroni) '(macaroni and cheese))
; union: similar to makeset, we combine two sets and leave out duplicates
(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((null? set2) set1)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else (cons (car set1)
                  (union (cdr set1) set2))))))
;example:
(union '(stewed tomatoes and macaroni casserole) '(macaroni and cheese))
