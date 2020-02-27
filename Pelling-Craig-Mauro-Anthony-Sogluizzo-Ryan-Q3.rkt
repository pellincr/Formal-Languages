#lang racket
(require fsm)
(require test-engine/racket-tests)
;STEPS
;Step 1: find all reachable states that have a path to the final states
;Step 2: remove all states that are not a memeber of the list from step 1
;Step 3: remove rules involved with states that are not on the list from step 1
;Step 4: ????
;Step 5: Profit


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
  (make-dfa '(Q0 Q1 Q2 Q3)
            '(a b)
            'Q0
            '(Q0 Q1)
            '((Q0 a Q1)
              (Q1 a Q2)
              (Q2 a Q1)
              (Q3 a Q3)
              (Q3 b Q3)
              (Q0 b Q2)
              (Q2 b Q1)
              (Q1 b Q0))))

(define TESTTWO-EQ
  (make-dfa '(Q0 Q1 Q2)
            '(a b)
            'Q0
            '(Q0 Q1)
            '((Q0 a Q1)
              (Q1 a Q2)
              (Q2 a Q1)
              (Q0 b Q2)
              (Q2 b Q1)
              (Q1 b Q0))))

(define TESTTHREE
  (make-dfa '(Q0 Q1 Q2 Q3)
            '(a b)
            'Q0
            '(Q1)
            '((Q0 a Q1)
              (Q1 b Q3)
              (Q3 b Q3)
              (Q2 b Q0))))
(define TESTTHREE-EQ
  (make-dfa '(Q0 Q1)
            '(a b)
            'Q0
            '(Q1)
            '((Q0 a Q1))))






;remove-unproductive-rules: a-dfa -> regular-grammar
;Purpose: to remove all unproductive "silly" rules and states from the given dfa and turn it into a grammar
(define (remove-unproductive a-dfa)
  ;INVENTORY
  ;(sm-getstates a-dfa) returns the states of the given dfa
  ;(sm-getalphabet a-dfa) returns the alphabet of the given dfa
  ;(sm-getstart a-dfa) returns the start state of the given dfa
  ;(sm-getfinals a-dfa) returns the final states of the given dfa
  ;(sm-getrules a-dfa) returns the rules of the given dfa
  (let ((new-states (update-states a-dfa))
        (new-alphabet (sm-getalphabet a-dfa))
        (new-start (sm-getstart a-dfa))
        (new-finals (sm-getfinals a-dfa))
        (new-rules (update-rules (sm-getrules a-dfa) (update-states a-dfa))))
    (sm->grammar
     (make-dfa
      new-states
      new-alphabet
      new-start
      new-finals
      new-rules
      'no-dead))))

(check-expect (grammar-testequiv (remove-unproductive TESTONE) (remove-unproductive TESTONE-EQ)) #t)
(check-expect (grammar-testequiv (remove-unproductive TESTONE) (sm->grammar TESTONE)) #t)
(check-expect (grammar-testequiv (remove-unproductive TESTONE) (sm->grammar TESTONE-EQ)) #t)
(check-expect (grammar-testequiv (remove-unproductive TESTONE-EQ) (sm->grammar TESTONE-EQ)) #t)
(check-expect (grammar-testequiv (remove-unproductive TESTTWO) (sm->grammar TESTTWO-EQ)) #t)
(check-expect (grammar-testequiv (remove-unproductive TESTTWO) (sm->grammar TESTTWO)) #t)
(check-expect (grammar-testequiv (remove-unproductive TESTTWO-EQ) (sm->grammar TESTTWO-EQ))#t)
(check-expect (grammar-testequiv (remove-unproductive TESTTHREE) (sm->grammar TESTTHREE-EQ)) #t)
(check-expect (grammar-testequiv (remove-unproductive TESTTHREE) (sm->grammar TESTTHREE))#t)
(check-expect (grammar-testequiv (remove-unproductive TESTTHREE-EQ) (sm->grammar TESTTHREE-EQ))#t)


;update-states: dfa -> (listof states)
;Purpose: to return the list of states without unreachable states and states that have no
;path to a final state
(define (update-states a-dfa)
  ;INVENTORY
  ;(sm-getstates a-dfa) returns the states of the given dfa
  ;(sm-getalphabet a-dfa) returns the alphabet of the given dfa
  ;(sm-getstart a-dfa) returns the start state of the given dfa
  ;(sm-getfinals a-dfa) returns the final states of the given dfa
  ;(sm-getrules a-dfa) returns the rules of the given dfa
  (let (;reachable: list of all reachable states
        (reachable (get-reachable a-dfa))
        ;finishables: list of all states that have a path to a final state
        (finishables (path-to-finish a-dfa (sm-getfinals a-dfa))))
    (filter (lambda (x) (member x reachable)) finishables)))

(check-expect (update-states TESTONE) '(Q0 Q1))
(check-expect (update-states TESTONE-EQ) '(Q0 Q1))
(check-expect (update-states TESTTWO) '(Q2 Q1 Q0))
(check-expect (update-states TESTTWO-EQ) '(Q2 Q1 Q0))
(check-expect (update-states TESTTHREE) '(Q0 Q1))
(check-expect (update-states TESTTHREE-EQ) '(Q0 Q1))





;update-rules: (listof rules) (listof states) -> (listof rules)
;Purpose: to remove any rule that is not connected to a state in te given list of states
(define (update-rules rules states)
  (cond [(null? rules) '()]
        [(or (not (member (caar rules) states)) (not (member (caddar rules) states)))
                                                                                (update-rules (cdr rules) states)]
        [else (cons (car rules) (update-rules (cdr rules) states))]))

(check-expect (update-rules (sm-getrules TESTONE) '(Q0 Q1))
              '((Q0 a Q1)
                (Q1 a Q0)
                (Q0 b Q0)
                (Q1 b Q1)))
(check-expect (update-rules (sm-getrules TESTONE-EQ) '(Q0 Q1))
              '((Q0 a Q1)
                (Q1 a Q0)
                (Q0 b Q0)
                (Q1 b Q1)))
(check-expect (update-rules (sm-getrules TESTTWO) '(Q0 Q1 Q2))
              '((Q0 a Q1)
              (Q1 a Q2)
              (Q2 a Q1)
              (Q0 b Q2)
              (Q2 b Q1)
              (Q1 b Q0)))
(check-expect (update-rules (sm-getrules TESTTWO-EQ) '(Q0 Q1 Q2))
              '((Q0 a Q1)
              (Q1 a Q2)
              (Q2 a Q1)
              (Q0 b Q2)
              (Q2 b Q1)
              (Q1 b Q0)))

(check-expect (update-rules (sm-getrules TESTTHREE) '(Q0 Q1))
              '((Q0 a Q1)))
(check-expect (update-rules (sm-getrules TESTTHREE-EQ) '(Q0 Q1))
              '((Q0 a Q1)))

;get-reachable: dfa-> (listof states)
;Purpose: to return the list of reachable states of te given dfa using breath-first-search
;ACCUM INV: the accumulator is all of the states that have been reached so far
(define (get-reachable dfa)
  ;INVENTORY
  ;(sm-getstates dfa) -> returns the states of the given machine
  ;(sm-getalphabet dfa) -> returns the alphabet of the given machine
  ;(sm-getrules dfa) -> returns the rules of the given machine
  ;(sm-getstart dfa) -> returns the start state of the given machine
  ;(sm-getfinals dfa) -> returns the final states of the machine
  (letrec
      ;reachable: (listof states) (listof states) ->(listof states)
      ;Purpose: to process through the dfa and return the reachable states
      ;ACCUM INV: the states that have been reached so far
      ((reachable (lambda(states-left reached)
                    (cond [(null? states-left) (remove-duplicates reached)]
                          [else
                           (let (;neighbors: generates a list of the neighboring states of the first of states-left without any state
                                 ;that already exists in the accumulator
                                 (neighbors (filter (lambda (x) (not (member x reached))) (generate-neighbors (sm-getrules dfa) (car states-left)))))
                             (reachable (append neighbors (cdr states-left)) (cons (car states-left) reached)))]))))
    (reachable (list(sm-getstart dfa)) '())))

(check-expect (get-reachable TESTONE) '(Q0 Q1))
(check-expect (get-reachable TESTONE-EQ) '(Q0 Q1))
(check-expect (get-reachable TESTTWO) '(Q2 Q1 Q0))
(check-expect (get-reachable TESTTWO-EQ) '(Q2 Q1 Q0))
(check-expect (get-reachable TESTTHREE)'(ds Q3 Q1 Q0))
(check-expect (get-reachable TESTTHREE-EQ) '(ds Q1 Q0))

;path-to-finish: dfa (listof states)-> (listof states)
;Purpose: returns all possible states that can reach the final state(s)
(define (path-to-finish dfa final-states)
  ;INVENTORY
  ;(sm-getstates dfa) -> returns the states of the given machine
  ;(sm-getalphabet dfa) -> returns the alphabet of the given machine
  ;(sm-getrules dfa) -> returns the rules of the given machine
  ;(sm-getstart dfa) -> returns the start state of the given machine
  ;(sm-getfinals dfa) -> returns the final states of the machine
  (letrec
      ;valid: (listof states) (listof states) -> (listof states)
      ;Purpose: to return the list of states the reach the given final state
      ;ACCUM INV: the states that can reach the final state
      ((valid (lambda (states-left current-valid-states)
                ;INVENTORY
                ;(sm-getstates dfa) -> returns the states of the given machine
                ;(sm-getalphabet dfa) -> returns the alphabet of the given machine
                ;(sm-getrules dfa) -> returns the rules of the given machine
                ;(sm-getstart dfa) -> returns the start state of the given machine
                ;(sm-getfinals dfa) -> returns the final states of the machine
                (cond [(null? states-left) (remove-duplicates current-valid-states)]
                      [else (letrec (;neighbors: the list of neighbors of the first of states-left without any that already
                                     ;exist in the accumulator
                                     (neighbors (filter (lambda (x) (not (member x current-valid-states)))
                                                        (generate-neighbors-reversed (sm-getrules dfa) (car states-left)))))
                              (valid (append neighbors (cdr states-left)) (cons (car states-left) current-valid-states)))]))))

    (cond [(null? (cdr final-states)) (valid (list(car final-states)) '())]
          [else (remove-duplicates (append (valid (list (car final-states)) '()) (path-to-finish dfa (cdr final-states))))])))

(check-expect (path-to-finish TESTONE (sm-getfinals TESTONE))
              '(Q0 Q1))
(check-expect (path-to-finish TESTONE-EQ (sm-getfinals TESTONE-EQ))
              '(Q0 Q1))

(check-expect (path-to-finish TESTTWO (sm-getfinals TESTTWO))
              '(Q2 Q1 Q0))
(check-expect (path-to-finish TESTTWO-EQ (sm-getfinals TESTTWO-EQ))
              '(Q2 Q1 Q0))

(check-expect (path-to-finish TESTTHREE (sm-getfinals TESTTHREE))
              '(Q2 Q0 Q1))
(check-expect (path-to-finish TESTTHREE-EQ (sm-getfinals TESTTHREE-EQ))
              '(Q0 Q1))




;generate-neighbors: (listof rules) symbol -> (listof-dfa)
;Purpose: to generate the neighbors of the given symbol in the given dfa
(define (generate-neighbors rules state)
  ;INVENTORY
  ;(sm-getstates dfa) -> returns the states of the given machine
  ;(sm-getalphabet dfa) -> returns the alphabet of the given machine
  ;(sm-getrules dfa) -> returns the rules of the given machine
  ;(sm-getstart dfa) -> returns the start state of the given machine
  ;(sm-getfinals dfa) -> returns the final states of the machine
  (cond [(null? rules) '()]
        [(eq? (caar rules) state) (cons (caddar rules) (filter (lambda (x) (not (eq? x (caddar rules))))
                                                               (generate-neighbors (cdr rules) state)))]
        [else (generate-neighbors (cdr rules) state)]))

(check-expect (generate-neighbors (sm-getrules TESTONE) 'Q0)
              '(Q1 Q0))
(check-expect (generate-neighbors (sm-getrules TESTONE-EQ) 'Q0)
              '(Q1 Q0))

(check-expect (generate-neighbors (sm-getrules TESTTWO) 'Q0)
              '(Q1 Q2))
(check-expect (generate-neighbors (sm-getrules TESTTWO-EQ) 'Q0)
              '(Q1 Q2))

(check-expect (generate-neighbors (sm-getrules TESTTHREE) 'Q0)
              '(Q1 ds))
(check-expect (generate-neighbors (sm-getrules TESTTHREE-EQ) 'Q0)
              '(Q1 ds))

;generate-neighbors-reversed: (listof rules) state -> (listof states)
;Purose: to generate the list of states that reach a given state
(define (generate-neighbors-reversed rules state)
  ;INVENTORY
  ;(sm-getstates dfa) -> returns the states of the given machine
  ;(sm-getalphabet dfa) -> returns the alphabet of the given machine
  ;(sm-getrules dfa) -> returns the rules of the given machine
  ;(sm-getstart dfa) -> returns the start state of the given machine
  ;(sm-getfinals dfa) -> returns the final states of the machine
  (cond [(null? rules) '()]
        [(eq? (caddar rules) state) (cons (caar rules) (filter (lambda (x) (not (eq? x (caar rules))))
                                                               (generate-neighbors-reversed (cdr rules) state)))]
        [else (generate-neighbors-reversed (cdr rules) state)]))

(check-expect (generate-neighbors-reversed (sm-getrules TESTONE) 'Q0) '(Q1 Q0))
(check-expect (generate-neighbors-reversed (sm-getrules TESTONE-EQ) 'Q0) '(Q1 Q0))

(check-expect (generate-neighbors-reversed (sm-getrules TESTTWO) 'Q0) '(Q1))
(check-expect (generate-neighbors-reversed (sm-getrules TESTTWO-EQ) 'Q0) '(Q1))

(check-expect (generate-neighbors-reversed (sm-getrules TESTTHREE) 'Q0) '(Q2))
(check-expect (generate-neighbors-reversed (sm-getrules TESTTHREE-EQ) 'Q1) '(Q0))

; ;Proof By induction
;
; Key: U = unproductive (silly)
; ; Say A and B are dfas
; ; Prove: (grammar-testequiv (remove-unproductive (dfa B))) == (grammar A)
; ;                            Where (dfa B) = {dfa A, (N)U} && (dfa A) has no unproductive rules
; ;
; ; Base Case: N=0, therefore the dfa B has no unproductive rules and is the same as dfa A
; ; Because dfa A and B are the same, their grammars will be the same and neither will have unproductive rules
; ;
; ; Assume: For some K where K = N, (grammar-testequiv (remove-unproductive (dfa B))) == (grammar A)
; ;                            Where (dfa B) = {dfa A, (N)U} && (dfa A) has no unproductive rules
; ;
; ; Prove: (grammar-testequiv (remove-unproductive (dfa B))) == (grammar A)
; ;                            Where (dfa B) = {dfa A, (K+1)U} && (dfa A) has no unproductive rules
; ;
; ; dfa B = {dfa A, (K+1)U} = {{dfa A, (K)U}, (1)U}
; ; The grammar (remove-unproductive ({dfa A, (K)U})) can be replaced with the grammar for dfa A since it is assumed after proving the base case
; ; {{dfa A, (K)U}, (1)U} = {{dfa A}, (1)U}
; ; because remove-unproductive recursivley removes all of the unproductive rules and states, any number of unproductive rules will be removed


(test)
