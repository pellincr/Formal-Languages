#lang racket
(require fsm)
(require test-engine/racket-tests)

;Exercise: 3.3.2 b
(define anbm
  (make-ndpda
   '(S M F)
   '(a b)
   '(a)
   'S
   '(F)
   `(((S ,EMP ,EMP) (M ,EMP))
     ((M ,EMP ,EMP) (F, EMP))
     ((M a ,EMP) (M(a a)))
     ((M a ,EMP) (M(a)))
     ((F b (a)) (F ,EMP))
     )))
;The a's must come before the b's
(check-expect (sm-apply anbm '()) 'accept)
(check-expect (sm-apply anbm '(a)) 'reject)
(check-expect (sm-apply anbm '(a b)) 'accept)
(check-expect (sm-apply anbm '(a a a a a b b b b)) 'reject)
(check-expect (sm-apply anbm '(a a b b b)) 'accept)
(check-expect (sm-apply anbm '(a a b b a b)) 'reject)

;S-INV
(define (S-INV ci s)
  (and (empty? ci) (empty? s)))
;M-INV
(define (M-INV ci s)
  (and (>= (length ci) 0)
       (andmap (lambda (x) (eqv? x 'a)) ci)))
;F-INV
(define (F-INV ci s)
  (and (>= (+ (length (filter (lambda (k) (eq? k 'b)) ci))(length s))
           (length (filter (lambda (k) (eq? k 'a)) ci)))
       (<= (+ (length (filter (lambda (k) (eq? k 'b)) ci))(length s))
           (* 2 (length (filter (lambda (k) (eq? k 'a)) ci))))))


;Question 2

;ndfa->ndpda: an-ndfa -> an-ndpda
;Purpose: to turn the given ndfa into an ndpda
(define (ndfa->ndpda an-ndfa)
  ;INVENTORY
  ;(sm-getstates an-ndfa) returns the states of the given dfa
  ;(sm-getalphabet an-ndfa) returns the alphabet of the given dfa
  ;(sm-getstart an-ndfa) returns the start state of the given dfa
  ;(sm-getfinals an-ndfa) returns the final states of the given dfa
  ;(sm-getrules an-ndfa) returns the rules of the given dfa
  (make-ndpda
   (sm-getstates an-ndfa)
   (sm-getalphabet an-ndfa)
   '()
   (sm-getstart an-ndfa)
   (sm-getfinals an-ndfa)
   (sm-rules->ndpda-rules (sm-getrules an-ndfa))
   )
  )

;sm-rules->ndpda-rules: (listof ndfa rules) -> (listof ndpda rules)
;purpose: to convert the given list of ndfa rules into a list of ndpda rules
;Always push empty to the stack
(define (sm-rules->ndpda-rules rules)
  (map sm-rule->ndpda-rule rules))

;sm-rule->ndpda-rule: ndfarule -> ndpdarule
;Purpose: to convert a given ndfa rule into an ndpda rule
(define (sm-rule->ndpda-rule rule)
  ;INVENTORY
  ;(car rule) -> current state
  ;(cadr rule) -> next-input
  ;(caddr rule) -> next-state
  (list (list (car rule) (cadr rule) '()) (list (caddr rule) '())))

(test)
