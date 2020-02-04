#lang racket
(require test-engine/racket-tests)
(require fsm)

(define TESTONE
  (make-dfa '(Q0 Q1 Q3)
            '(a b)
            'Q0
            '(Q0 Q1)
            '((Q0 a Q1)
              (Q1 a Q0)
              (Q3 a Q3)
              (Q0 b Q0)
              (Q1 b Q1)
              (Q3 b Q3))))

(define TESTONE-EQ
  (make-dfa '(Q0 Q1)
            '(a b)
            'Q0
            '(Q0 Q1)
            '((Q0 a Q1)
              (Q1 a Q0)
              (Q0 b Q0)
              (Q1 b Q1))))

(define TESTTWO
  (make-dfa '(Q0 Q1 Q2 Q3 Q4 Q5)
            '(a b)
            'Q1
            '(Q0 Q4 Q5)
            '((Q0 a Q5)
              (Q1 a Q2)
              (Q2 a Q3)
              (Q3 a Q4)
              (Q4 a Q4)
              (Q5 a Q0)
              (Q0 b Q5)
              (Q1 b Q3)
              (Q3 b Q2)
              (Q2 b Q4)
              (Q4 b Q4)
              (Q5 b Q0)
              )))


(define TESTTWO-EQ
  (make-dfa '(Q1 Q2 Q3 Q4)
            '(a b)
            'Q1
            '(Q4)
            '((Q1 a Q2)
              (Q2 a Q3)
              (Q3 a Q4)
              (Q4 a Q4)
              (Q1 b Q3)
              (Q3 b Q2)
              (Q2 b Q4)
              (Q4 b Q4))))

(define TESTTHREE
  (make-dfa '(Q0 Q1 Q2 Q3 Q4 Q5)
            '(a b)
            'Q0
            '(Q5)
            '((Q0 a Q1)
              (Q1 a Q3)
              (Q3 a Q5)
              (Q5 a Q5)
              (Q2 a Q1)
              (Q4 a Q3)
              (Q0 b Q3)
              (Q3 b Q1)
              (Q1 b Q5)
              (Q5 b Q5)
              (Q2 b Q2)
              (Q4 b Q4))))

(define TESTTHREE-EQ
  (make-dfa '(Q0 Q1 Q3 Q5)
            '(a b)
            'Q0
            '(Q5)
            '((Q0 a Q1)
              (Q1 a Q3)
              (Q3 a Q5)
              (Q5 a Q5)
              (Q0 b Q3)
              (Q3 b Q1)
              (Q1 b Q5)
              (Q5 b Q5))))



; Proof by induction
;
; KEY: U == nonreachable state
;
; Say A and B are dfas
; Prove: (remove-nonreachable (dfa B)) == (dfaA)
;                            Where (dfa B) = {dfa A, (N)U} && (dfa A) has no nonreachale states
;
; Base Case: N = 0, therefore, dfa B contains no nonreachable states, therefore (dfa B) == (dfa A), therefore (remove-nonreachable (dfa B)) == (dfa A)
;
; Assume: For some K where K = N, (remove-nonreachable (dfa B) == (dfa A))
;                            Where (dfa B) = {dfa A, (K)U} && (dfa A) has no nonreachale states
;
; Prove: (remove-nonreachable (dfa B)) == (dfa A)
;                            Where (dfa B) = {dfa A, (K+1)U} && (dfa A) has no nonreachale states
;
; dfa B = {dfa A, (K+1)U} = {{dfa A, (K)U}, (1)U} = {dfa A,(1)U}


;remove-nonreachable: dfa -> dfa
;Purpose: to remove the non-reachable states from the given dfa
(define (remove-nonreachable dfa)
  ;INVENTORY
  ;(sm-getstates dfa) -> returns the states of the machine
  ;(sm-getalphabet dfa) -> retruns the alphabet of the given state
  ;(sm-getrules dfa) -> returns the rules of the given dfa
  ;(sm-getstart dfa) -> retruns the starting state
  ;(sm-getfinals dfa) -> returns the list of final states
  (make-dfa
   (remove-nonreachable-states (sm-getrules dfa) (list(sm-getstart dfa)))
   (sm-getalphabet dfa)
   (sm-getstart dfa)
   (sm-getfinals dfa)
   (update-rules (sm-getrules dfa) (filter
                                    (lambda (x) (member x (remove-nonreachable-states (sm-getrules dfa) (list (sm-getstart dfa)))))
                                    (sm-getstates dfa)))))

;update-rules: list-of-rules list-of-states -> list-of-rules
;Purpose: to remove any rules that have to do with an unreachable state
(define (update-rules rules non-reachable-states)
  (cond [(null? rules) rules]
        [(or (member (caar rules) non-reachable-states) (member (caddar rules) non-reachable-states))
         (update-rules (cdr rules) non-reachable-states)]
        [else (cons (car rules) (update-rules (cdr rules) non-reachable-states))]))

(check-expect (update-rules '((Q0 a Q1)
                              (Q1 a Q0)
                              (Q3 a Q3)) '(Q3))
              '((Q0 a Q1)
                (Q1 a Q0)))


;remove-noneachable-states: list-of-rules list-of-state -> list-of-states
;Purpose: to return the list of states that are reachable given the rules
;ACCUM INV:
(define (remove-nonreachable-states rules reached-states)
  ;INVENTORY
  ;(sm-getstates dfa) -> returns the states of the machine
  ;(sm-getalphabet dfa) -> retruns the alphabet of the given state
  ;(sm-getrules dfa) -> returns the rules of the given dfa
  ;(sm-getstart dfa) -> retruns the starting state
  ;(sm-getfinals dfa) -> returns the list of final states
  (cond [(null? rules) (reverse reached-states)]
        [(and (member (caar rules) reached-states) (not (member (caddar rules) reached-states)))
         (remove-nonreachable-states (cdr rules) (cons (caddar rules) reached-states))]
        [else (remove-nonreachable-states (cdr rules) reached-states)]))


(check-expect (remove-nonreachable-states '((Q0 a Q1)
                                           (Q1 a Q0)
                                           (Q3 a Q3)
                                           (Q0 b Q0)
                                           (Q1 b Q1)
                                           (Q3 b Q3)) '(Q0))
              '(Q0 Q1))


(sm-testequiv? TESTONE TESTONE-EQ)
(sm-testequiv? TESTTWO TESTTWO-EQ)
(sm-testequiv? TESTTHREE TESTTHREE-EQ)







(test)
