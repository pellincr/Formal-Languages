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
  (eq? (list-ref t (- i 1)) 'a))

(define (N-INV t i)
 (not (contains-aa t)))

(define (S-INV t i)
 #t)

(define (Y-INV t i)
  (and (eq? (list-ref t i) 'a)
       (eq? (list-ref t (- i 1)) 'a)))

(define (contains-aa t)
  (cond [(or (empty? t) (empty? (cdr t))) #f]
        [(and (eq? (car t) 'a) (eq? (cadr t) 'a)) #t]
        [else (contains-aa (cdr t))]))

(check-expect (contains-aa '(a b a b b a)) #f)
(check-expect (contains-aa '(a b a a)) #t)
(check-expect (contains-aa '(a b a)) #f)
(check-expect (contains-aa '(a a)) #t)
(check-expect (contains-aa '(a)) #f)
(check-expect (contains-aa '()) #f)

; Exercise 4.18c


(define L-R
  (make-tm
   '(L R)
   '()
   `(((L ,BLANK) (R ,BLANK)))
   'L
   '(R)
   'R))

(define partc
  (make-tm
   '(Q0 Q1 H0 H1)
   '(a b)
   `(((Q0 a) (H0 ,LEFT))
     ((Q0 b) (H0 ,LEFT))
     ((Q0 ,BLANK) (Q1 ,LEFT))
     ((Q1 a) (H1 ,RIGHT))
     ((Q1 b) (H1 ,RIGHT))
     ((Q1 ,BLANK) (H1, RIGHT)))
   'Q0
   '(H0 H1)
   'H1))

(test)
