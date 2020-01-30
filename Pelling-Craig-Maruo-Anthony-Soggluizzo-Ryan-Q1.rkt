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
   (sm-getrules dfa)))


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




(check-expect (sm-testequiv? (remove-nonreachable TESTONE) TESTONE-EQ) #t)









(test)
