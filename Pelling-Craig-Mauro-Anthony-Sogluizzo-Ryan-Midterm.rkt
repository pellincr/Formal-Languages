#lang racket

(require fsm)
(require test-engine/racket-tests)
; ; Design and implement in FSM a finite state machine to detect if bbabbab is a substring
; ; in a word w. Follow all the steps of the design recipe. Make sure to use the state
; ; invariants you develop to prove that your machine works.
;


(define bbabbab (make-dfa
                  '(Q0 Q1 Q2 Q3 Q4 Q5 Q6 Q7)
                  '(a b)
                  'Q0
                  '(Q7)
                  '((Q0 a Q0)
                    (Q0 b Q1)
                    (Q1 a Q0)
                    (Q1 b Q2)
                    (Q2 a Q3)
                    (Q2 b Q2)
                    (Q3 a Q0)
                    (Q3 b Q4)
                    (Q4 a Q0)
                    (Q4 b Q5)
                    (Q5 b Q2)
                    (Q5 a Q6)
                    (Q6 a Q0)
                    (Q6 b Q7)
                    (Q7 a Q7)
                    (Q7 b Q7))))


(check-expect (sm-apply bbabbab '()) 'reject)
(check-expect (sm-apply bbabbab '(a)) 'reject)
(check-expect (sm-apply bbabbab '(b b a b b a b)) 'accept)
(check-expect (sm-apply bbabbab '(a b a a a b b b a b b a b)) 'accept)
(check-expect (sm-apply bbabbab '(b b a b b a b a b b a b a b b a)) 'accept)
(check-expect (sm-apply bbabbab '(b b b b b b b b a b b a b b b b b b b a a a)) 'accept)
(check-expect (sm-apply bbabbab '(b b a b a b)) 'reject)
; ; The sketch of the proof that finite state machines are closed under union developed
; ; in class stated:
; ;
; ; .
; ;
; ; Prove the above by induction on the length of w.
;



(test)
