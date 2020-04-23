#lang racket
(require fsm)
(require test-engine/racket-tests)

; Exercise 4.17

(define aa
    (make-tm
   '(S A Y N)
   '(a b)
   `(((S a) (A ,RIGHT))
     ((S b) (S ,RIGHT))
     ((S ,BLANK) (N ,BLANK))
     ((A a) (Y a))
     ((A b) (S ,RIGHT))
     ((A ,BLANK) (N ,BLANK)))
   'S
   '(Y N)
   'Y))
(check-expect (sm-apply aa `(,LM a b b a b a b a a)) 'accept)
(check-expect (sm-apply aa `(,LM b b)) 'reject)
(check-expect (sm-apply aa `(,LM a a b b b b)) 'accept)
(check-expect (sm-apply aa `(,LM a b b a)) 'reject)
(check-expect (sm-apply aa `(,LM a b b)) 'reject)
(check-expect (sm-apply aa `(,LM a)) 'reject)

(define (A-INV t i)
  (eq? (list-ref t i) 'a))

(define (Y-INV t i)
  (and (eq? (list-ref t i) 'a)
       (eq? (list-ref t (- i 1)) 'a)))

; Exercise 4.18c


(define L-R
  (make-tm
   '(L R)
   '()
   `(((L ,BLANK) (R ,BLANK)))
   'L
   '(R)
   'R))

(test)
