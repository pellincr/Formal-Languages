#lang racket
(require fsm)
(require test-engine/racket-tests)

; TEMPLATE
; ;f-on-ndfa: ndfa -> ???
; ;Purpose: ???
; (define (f-on-ndfa ndfa)
;   ;INVENTORY
;   ;(sm-getstates ndfa) -> returns the states of the given machine
;   ;(sm-getalphabet ndfa) -> returns the alphabet of the given machine
;   ;(sm-getrules ndfa) -> returns the rules of the given machine
;   ;(sm-getstart ndfa) -> returns the start state of the given machine
;   ;(sm-getfinals ndfa) -> returns the final states of the machine
;   ...)

;Prove that a regular language is closed under Prefix

;NDFA EXAMPLES
(define KLEENESTAR-abUaba
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

(define KLEENESTAR-abUaba-2
  (make-ndfa '(Q-0 Q-1 Q-2 Q-3 Q-4 Q-5 Q-6)
             '(a b)
             'Q-0
             '(Q-0)
             `((Q-0 a Q-1)
               (Q-1 b Q-2)
               (Q-2 a Q-3)
               (Q-3 ,EMP Q-0)
               (Q-3 a Q-6)
               (Q-0 a Q-4)
               (Q-4 b Q-5)
               (Q-5 ,EMP Q-0))))

(define KLEENESTAR-abUaba-3
  (make-ndfa '(Q-0 Q-1 Q-2 Q-3 Q-4 Q-5 Q-6)
             '(a b)
             'Q-0
             '(Q-0)
             `((Q-0 a Q-1)
               (Q-1 b Q-2)
               (Q-2 a Q-3)
               (Q-3 ,EMP Q-0)
               (Q-0 a Q-4)
               (Q-4 b Q-5)
               (Q-6 a Q-6)
               (Q-6 b Q-6)
               (Q-5 ,EMP Q-0))))

(define KLEENESTAR-bbUba
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




;accept-prefixes: ndfa -> ndfa
;Purpose: to build an dfa that accepts all Prefixs of the initial dfa
(define (accept-prefixes ndfa)
  ;INVENTORY
  ;(sm-getstates ndfa) -> returns the states of the given machine
  ;(sm-getalphabet ndfa) -> returns the alphabet of the given machine
  ;(sm-getrules ndfa) -> returns the rules of the given machine
  ;(sm-getstart ndfa) -> returns the start state of the given machine
  ;(sm-getfinals ndfa) -> returns the final states of the machine
  (let (;The list of all states that have a path to the final state(s)
        (pathable-states (path-to-finish ndfa (sm-getfinals ndfa)))
        ;The list of all reachable states
        (reachable-states (get-reachable ndfa)))
    (make-ndfa (sm-getstates ndfa)
               (sm-getalphabet ndfa)
               (sm-getstart ndfa)
               (filter (lambda (x) (member x pathable-states)) reachable-states)
               (sm-getrules ndfa))))



;prefix-check: ndfa word -> boolean
;Purpose: to determine if all prefixes of the given word are accepted
(define (prefix-check ndfa word)
  ;INVENTORY
  ;(sm-getstates ndfa) -> returns the states of the given machine
  ;(sm-getalphabet ndfa) -> returns the alphabet of the given machine
  ;(sm-getrules ndfa) -> returns the rules of the given machine
  ;(sm-getstart ndfa) -> returns the start state of the given machine
  ;(sm-getfinals ndfa) -> returns the final states of the machine
  (cond [(and (null? word) (eq? (sm-apply ndfa word) 'accept)) #t]
        [else (and (eq? (sm-apply ndfa word) 'accept) (prefix-check ndfa (reverse (cdr (reverse word)))))]))

(check-expect (prefix-check (accept-prefixes KLEENESTAR-abUaba) '()) #t)
(check-expect (prefix-check (accept-prefixes KLEENESTAR-abUaba) '(a)) #t)
(check-expect (prefix-check (accept-prefixes KLEENESTAR-abUaba) '(a b)) #t)
(check-expect (prefix-check (accept-prefixes KLEENESTAR-abUaba) '(a b a)) #t)
(check-expect (prefix-check (accept-prefixes KLEENESTAR-abUaba) '(a b a b)) #t)
(check-expect (prefix-check (accept-prefixes KLEENESTAR-abUaba) '(a b a b b)) #f)

(check-expect (prefix-check (accept-prefixes KLEENESTAR-abUaba-3) '()) #t)
(check-expect (prefix-check (accept-prefixes KLEENESTAR-abUaba-3) '(a)) #t)
(check-expect (prefix-check (accept-prefixes KLEENESTAR-abUaba-3) '(a b)) #t)
(check-expect (prefix-check (accept-prefixes KLEENESTAR-abUaba-3) '(a b a)) #t)
(check-expect (prefix-check (accept-prefixes KLEENESTAR-abUaba-3) '(a b a b)) #t)
(check-expect (prefix-check (accept-prefixes KLEENESTAR-abUaba-3) '(a b a b a)) #t)
(check-expect (prefix-check (accept-prefixes KLEENESTAR-abUaba-3) '(a b a b a a)) #t)
(check-expect (prefix-check (accept-prefixes KLEENESTAR-abUaba-3) '(a b a b a a a)) #f)

(check-expect (prefix-check (accept-prefixes KLEENESTAR-bbUba) '()) #t)
(check-expect (prefix-check (accept-prefixes KLEENESTAR-bbUba) '(b)) #t)
(check-expect (prefix-check (accept-prefixes KLEENESTAR-bbUba) '(b b)) #t)
(check-expect (prefix-check (accept-prefixes KLEENESTAR-bbUba) '(b b b)) #t)
(check-expect (prefix-check (accept-prefixes KLEENESTAR-bbUba) '(b b b a)) #t)
(check-expect (prefix-check (accept-prefixes KLEENESTAR-bbUba) '(a b a b)) #f)




;get-reachable: ndfa-> list-of-states
;Purpose: to return the list of reachable states of te given ndfa using breath-first-search
;ACCUM INV: the accumulator is all of the states that have been reached so far
(define (get-reachable ndfa)
  ;INVENTORY
  ;(sm-getstates ndfa) -> returns the states of the given machine
  ;(sm-getalphabet ndfa) -> returns the alphabet of the given machine
  ;(sm-getrules ndfa) -> returns the rules of the given machine
  ;(sm-getstart ndfa) -> returns the start state of the given machine
  ;(sm-getfinals ndfa) -> returns the final states of the machine
  (letrec
      ;reachable: (listof states) (listof states) ->(listof states)
      ;Purpose: to process through the ndfa and return the reachable states
      ;ACCUM INV: the states that have been reached so far
      ((reachable (lambda(states-left reached)
                    (cond [(null? states-left) reached]
                          [else
                           (let (;neighbors: generates a list of the neighboring states of the first of states-left without any state
                                 ;that already exists in the accumulator
                                 (neighbors (filter (lambda (x) (not (member x reached))) (generate-neighbors (sm-getrules ndfa) (car states-left)))))
                             (reachable (append neighbors (cdr states-left)) (cons (car states-left) reached)))]))))
    (reachable (list(sm-getstart ndfa)) '())))

(check-expect (get-reachable KLEENESTAR-abUaba)
              '(Q-5 Q-4 Q-3 Q-2 Q-1 Q-0))
(check-expect (get-reachable KLEENESTAR-abUaba-2)
              '(Q-5 Q-4 Q-6 Q-3 Q-2 Q-1 Q-0))
(check-expect (get-reachable KLEENESTAR-abUaba-3)
              '(Q-5 Q-4 Q-3 Q-2 Q-1 Q-0))

(check-expect (get-reachable KLEENESTAR-bbUba)
              '(Q-4 Q-3 Q-2 Q-1 Q-0))


;path-to-finish: ndfa (listof states)-> (listof states)
;Purpose: returns all possible states that can reach the final state(s)
(define (path-to-finish ndfa final-states)
  ;INVENTORY
  ;(sm-getstates ndfa) -> returns the states of the given machine
  ;(sm-getalphabet ndfa) -> returns the alphabet of the given machine
  ;(sm-getrules ndfa) -> returns the rules of the given machine
  ;(sm-getstart ndfa) -> returns the start state of the given machine
  ;(sm-getfinals ndfa) -> returns the final states of the machine
  (letrec
      ;valid: (listof states) (listof states) -> (listof states)
      ;Purpose: to return the list of states the reach the given final state
      ;ACCUM INV: the states that can reach the final state
      ((valid (lambda (states-left current-valid-states)
                ;INVENTORY
                ;(sm-getstates ndfa) -> returns the states of the given machine
                ;(sm-getalphabet ndfa) -> returns the alphabet of the given machine
                ;(sm-getrules ndfa) -> returns the rules of the given machine
                ;(sm-getstart ndfa) -> returns the start state of the given machine
                ;(sm-getfinals ndfa) -> returns the final states of the machine
                (cond [(null? states-left) current-valid-states]
                      [else (letrec (;neighbors: the list of neighbors of the first of states-left without any that already
                                     ;exist in the accumulator
                                     (neighbors (filter (lambda (x) (not (member x current-valid-states)))
                                                        (generate-neighbors-reversed (sm-getrules ndfa) (car states-left)))))
                              (valid (append neighbors (cdr states-left)) (cons (car states-left) current-valid-states)))]))))

    (cond [(null? (cdr final-states)) (valid (list(car final-states)) '())]
          [else (append (valid (list (car final-states)) '()) (path-to-finish ndfa (cdr final-states)))])))

(check-expect (path-to-finish KLEENESTAR-abUaba '(Q-0))
              '(Q-4 Q-5 Q-1 Q-2 Q-3 Q-0))
(check-expect (path-to-finish KLEENESTAR-abUaba-2 '(Q-0))
              '(Q-4 Q-5 Q-1 Q-2 Q-3 Q-0))
(check-expect (path-to-finish KLEENESTAR-abUaba-3 '(Q-0))
              '(Q-4 Q-5 Q-1 Q-2 Q-3 Q-0))
(check-expect (path-to-finish KLEENESTAR-bbUba '(Q-0))
              '(Q-3 Q-4 Q-1 Q-2 Q-0))


;generate-neighbors: (listof rules) symbol -> (listof-ndfa)
;Purpose: to generate the neighbors of the given symbol in the given ndfa
(define (generate-neighbors rules state)
  ;INVENTORY
  ;(sm-getstates ndfa) -> returns the states of the given machine
  ;(sm-getalphabet ndfa) -> returns the alphabet of the given machine
  ;(sm-getrules ndfa) -> returns the rules of the given machine
  ;(sm-getstart ndfa) -> returns the start state of the given machine
  ;(sm-getfinals ndfa) -> returns the final states of the machine
  (cond [(null? rules) '()]
        [(eq? (caar rules) state) (cons (caddar rules) (filter (lambda (x) (not (eq? x (caddar rules))))
                                                               (generate-neighbors (cdr rules) state)))]
        [else (generate-neighbors (cdr rules) state)]))

(check-expect (generate-neighbors '((A a B)
                                    (A b A)
                                    (B b A)
                                    (B a B))
                                  'A)
              '(B A))

(check-expect (generate-neighbors '() 'A) '())
(check-expect (generate-neighbors '((A a B)
                                    (A b A)
                                    (A c C)
                                    (B a B)
                                    (B b A)
                                    (B c C)
                                    (C a C)
                                    (C b C)
                                    (C c C))
                                  'C)
              '(C))


;generate-neighbors-reversed: (listof rules) state -> (listof states)
;Purose: to generate the list of states that reach a given state
(define (generate-neighbors-reversed rules state)
  ;INVENTORY
  ;(sm-getstates ndfa) -> returns the states of the given machine
  ;(sm-getalphabet ndfa) -> returns the alphabet of the given machine
  ;(sm-getrules ndfa) -> returns the rules of the given machine
  ;(sm-getstart ndfa) -> returns the start state of the given machine
  ;(sm-getfinals ndfa) -> returns the final states of the machine
  (cond [(null? rules) '()]
        [(eq? (caddar rules) state) (cons (caar rules) (filter (lambda (x) (not (eq? x (caar rules))))
                                                               (generate-neighbors-reversed (cdr rules) state)))]
        [else (generate-neighbors-reversed (cdr rules) state)]))

(check-expect (generate-neighbors-reversed '((A a B)
                                             (A b B)
                                             (A c C)
                                             (B a A)
                                             (B b B)
                                             (B c C)
                                             (C a C)
                                             (C b C)
                                             (C c C))
                                           'C)
              '(A B C))

(test)
; ;Proof by induction
; ;Prove: a regular language is closed under Prefix.
; ;Let L be a regular language such that L = L(M).
; ;Prefix(L) = {v | w E L and v is a prefix of w}
;
; Key: e = empty = ()
; ;Base case: L = e
; ;An empty language can only contain words that are ()
; ;any word must be empty and will only have one prefix which is itself, the empty string e
; ;the only v must also be empty which will be a prefix of w where w E L
;
; ;Assume: Prefix(L) = {v | w E L and v is a prefix of w}
;
; ;Prove: for some k where k = w
; ;Prefix(L) = {v | k+1 E L and v is a prefix of k+1} where k+1 is a word that contains k, followed by an extra letter in the alphabet
;
; ;k is a prefix of the word k+1
; ;v is known to be a prefix of any word k
; ;by definition of a prefix, k would be a prefix of k+1
; ;based on what was already assumed, v is known to be a prefix of k
; ;therefore, v is a prefix of a prefix of k+1, so v must be a prefix of k+1
