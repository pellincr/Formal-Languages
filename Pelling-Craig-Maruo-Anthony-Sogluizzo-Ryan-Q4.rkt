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
  (list (list (car rule) (cadr rule) EMP) (list (caddr rule) EMP)))



;;;;;;;;;;;;;;;TESTS;;;;;;;;;;;;;;;;;;;;
(define TESTONE-NDFA
  (make-ndfa '(Q-0 Q-1 Q-2 Q-3 Q-4 Q-5)
             '(a b)
             'Q-0
             '(Q-0)
             `((Q-0 a Q-1)
               (Q-1 b Q-2)
               (Q-2 a Q-3)
               (Q-3 ,EMP Q-0)
               (Q-0 a Q-4)
               (Q-4 b Q-5)
               (Q-5 ,EMP Q-0))))

(define TESTTWO-NDFA
  (make-ndfa '(Q-0 Q-1 Q-2 Q-3 Q-4)
             '(a b)
             'Q-0
             '(Q-0)
             `((Q-0 b Q-1)
               (Q-1 b Q-2)
               (Q-2 ,EMP Q-0)
               (Q-0 b Q-3)
               (Q-3 a Q-4)
               (Q-4 ,EMP Q-0))))





(check-expect (sm-testequiv? TESTONE-NDFA (ndfa->ndpda TESTONE-NDFA))#true)
(check-expect (sm-testequiv? TESTTWO-NDFA (ndfa->ndpda TESTTWO-NDFA))#true)
             
              



(check-expect(sm-rule->ndpda-rule '(Q-5 ,EMP Q-0))'((Q-5 ,EMP e) (Q-0 e)))  ;;last rule of TESTONE-NDFA
(check-expect(sm-rule->ndpda-rule  '(Q-0 b Q-3))'((Q-0 b e) (Q-3 e))) ;; fourth rule of TESTTWO-NDFA


(check-expect (sm-rules->ndpda-rules  `((Q-0 a Q-1)
               (Q-1 b Q-2)
               (Q-2 a Q-3)
               (Q-3 ,EMP Q-0)
               (Q-0 a Q-4)
               (Q-4 b Q-5)
               (Q-5 ,EMP Q-0)))'(((Q-0 a e) (Q-1 e)) ((Q-1 b e) (Q-2 e)) ((Q-2 a e) (Q-3 e)) ((Q-3 e e) (Q-0 e)) ((Q-0 a e) (Q-4 e)) ((Q-4 b e) (Q-5 e)) ((Q-5 e e) (Q-0 e)))) ;;all translated rules for TESTONE-NDFA

(check-expect (sm-rules->ndpda-rules `((Q-0 b Q-1)
               (Q-1 b Q-2)
               (Q-2 ,EMP Q-0)
               (Q-0 b Q-3)
               (Q-3 a Q-4)
               (Q-4 ,EMP Q-0)))'(((Q-0 b e) (Q-1 e)) ((Q-1 b e) (Q-2 e)) ((Q-2 e e) (Q-0 e)) ((Q-0 b e) (Q-3 e)) ((Q-3 a e) (Q-4 e)) ((Q-4 e e) (Q-0 e))))  ;;all translated rules for TESTTWO-NDFA
 
(test)
