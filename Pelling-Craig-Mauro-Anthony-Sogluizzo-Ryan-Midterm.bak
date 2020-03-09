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


;has-word?: listof-chars listof-chars-> boolean
;Purpose: to determine if the given list of characters has the other list of characters within it
(define (has-word? input expected)
  (cond [(< (length input) (length expected)) #f]
        [(equal? (first-n-elements input (length expected)) expected) #t]
        [else (has-word? (cdr input) expected)]))

(check-expect (has-word? '(a b b a b a) '(b b a)) #t)
(check-expect (has-word? '(a b b a b a) '(b b b)) #f)
(check-expect (has-word? '(a b a b) '()) #t)

;first-n-elements: list num -> list
;Purpose: to output a list of the first n elements of the given list
(define (first-n-elements loe n)
  ;INVENTORY
  ;(car loe) returns the first of the loe
  ;(cdr loe) returns the rest of the loe
  (cond [(= n 0) '()]
        [else (cons (car loe) (first-n-elements (cdr loe) (- n 1)))]))

(check-expect (first-n-elements '(a b b a b) 3) '(a b b))
(check-expect (first-n-elements '(a b b a b a) 0) '())



;Q0 Invariant
(define (Q0-INV consumed-input)
  (equal? (first-n-elements (reverse consumed-input) 0) '()))
;Q1 Invariant
(define (Q1-INV consumed-input)
  (equal? (first-n-elements (reverse consumed-input) 1) '(b)))
;Q2 Invariant
(define (Q2-INV consumed-input)
  (equal? (first-n-elements (reverse consumed-input) 2) '(b b)))
;Q3 Invariant
(define (Q3-INV consumed-input)
  (equal? (first-n-elements (reverse consumed-input) 3) '(a b b)))
;Q4 Invariant
(define (Q4-INV consumed-input)
  (equal? (first-n-elements (reverse consumed-input) 4) '(b a b b)))
;Q5 Invariant
(define (Q5-INV consumed-input)
  (equal? (first-n-elements (reverse consumed-input) 5) '(b b a b b)))
;Q6 Invariant
(define (Q6-INV consumed-input)
  (equal? (first-n-elements (reverse consumed-input) 6) '(a b b a b b)))
;Q7 Invariant
(define (Q7-INV consumed-input)
  (has-word? consumed-input '(b b a b b a b)))

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
