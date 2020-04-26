#lang racket
(require fsm)
(require test-engine/racket-tests)

; Exercise 4.17


(define aa
  (make-tm
   '(S A Y)
   `(a b ,LM)
   `(((S a) (A ,RIGHT))
     ((S b) (S ,RIGHT))
     ((S ,BLANK) (S ,RIGHT))
     ((A a) (Y a))
     ((A b) (S ,RIGHT))
     ((A ,BLANK) (S ,RIGHT)))
   'S
   '(Y)
   'Y))

(define test1-aa (sm-showtransitions aa `(,LM a b a a)))
(define test2-aa (sm-showtransitions aa `(,LM a b b a b a b a a)))
(define test3-aa (sm-showtransitions aa `(,LM a a b b b b)))
(define test4-aa (sm-showtransitions aa `(,LM a ,BLANK a a)))
(define test5-aa (sm-showtransitions aa `(,LM a b b a ,BLANK b b a a b)))
(check-expect (last test1-aa) '(Y 4 (@ a b a a)))
(check-expect (last test2-aa)'(Y 9 (@ a b b a b a b a a)))
(check-expect (last test3-aa)'(Y 2 (@ a a b b b b)))
(check-expect (last test4-aa)'(Y 4 (@ a _ a a)))
(check-expect (last test5-aa)'(Y 9 (@ a b b a _ b b a a b)))


(define (S-INV t i)
  #t)

(define (A-INV t i)
  (eq? (list-ref t (- i 1)) 'a))

(define (Y-INV t i)
  (and (eq? (list-ref t i) 'a)
       (eq? (list-ref t (- i 1)) 'a)))

; Exercise 4.18c

(define R
  (make-tm
   '(S H)
   '(a b)
   `(((S ,LM) (S ,RIGHT))
     ((S a) (H ,RIGHT))
     ((S b) (H ,RIGHT))
     ((S ,BLANK) (H ,RIGHT)))
   'S
   '(H)))

(define L
  (make-tm
   '(S H)
   '(a b)
   `(((S ,LM) (S ,RIGHT))
     ((S a) (H ,LEFT))
     ((S b) (H ,LEFT))
     ((S ,BLANK) (H ,LEFT)))
   'S
   '(H)))

(define LBR
  (make-tm
   '(S H)
   '(a b)
   `(((S ,LM) (S ,RIGHT))
     ((S a) (H ,LEFT))
     ((S b) (H ,LEFT))
     ((S ,BLANK) (H ,RIGHT)))
   'S
   '(H)))


(define HALT (make-tm '(S)
                      '(a b)
                      '()
                      'S
                      '(S)))

(define FBL (combine-tms
             (list 0 L (cons BRANCH
                              (list (list 'a (list GOTO 0))
                                    (list 'b (list GOTO 0))
                                    (list LM (list GOTO 0))
                                    (list BLANK HALT))))

             (list 'a 'b LM)))

(define LR (combine-tms
            (list FBL (cons BRANCH
                            (list (list 'a (list GOTO HALT))
                                  (list 'b (list GOTO HALT))
                                  (list LM (list GOTO HALT))
                                  (list BLANK R))))
            (list 'a 'b LM)))

(test)
