; Mitran Andrei-Gabriel, 323CA, 2023

#lang racket

(provide (all-defined-out))

; Funcție care primește lista preferințelor
; bărbaților și calculează lista bărbaților din problemă.
; Folosește recursivitate pe stivă.
(define (get-men mpref)
  (if (null? mpref)
      '()
      (cons (caar mpref) (get-men (cdr mpref)))))


; Funcție care primește lista preferințelor
; femeilor și calculează lista femeilor din problemă.
; Folosește recursivitate pe coadă.

; Wrapper-ul funcției helper
(define (get-women wpref)
  (get-women-helper wpref '()))

; Funcție helper, care implementează ceea ce se cere
(define (get-women-helper wpref acc)
  (if (null? wpref)
      (reverse acc)
      (get-women-helper (cdr wpref) (cons (caar wpref) acc))))


; Funcție recursivă care primește o listă de liste
; de preferințe (ori lista preferințelor bărbaților, ori a
; femeilor) și o persoană ale cărei preferințe apar în listă,
; și întoarce lista preferințelor acelei persoane.
; Se garantează că persoana apare în listă.
; Exemplu: dacă în lista wpref apare linia (ana bobo adi cos),
; (get-pref-list wpref 'ana) => '(bobo adi cos)
; Observație: de fiecare dată când ne referim la lista
; preferințelor unei persoane p, ne referim la o listă care conține
; doar persoanele de sex opus, nu și pe p pe prima poziție.
(define (get-pref-list pref person)
  (if (equal? (caar pref) person)
      (cdar pref)
      (get-pref-list (cdr pref) person)))


; Funcție recursivă care primește o listă de tipul
; întors la exercițiul precedent (lista preferințelor unei persoane),
; respectiv două persoane x și y care apar în lista respectivă,
; și întoarce true dacă x este mai sus decât y în topul preferințelor
; și false în caz contrar.
; Nu folosește operatori condiționali, folosește în schimb operatori
; logici pentru a obține același efect.
(define (preferable? pref-list x y)
  (or (equal? x (car pref-list)) (and (not (equal? y (car pref-list))) (preferable? (cdr pref-list) x y))))


; Funcție recursivă care primește o listă de logodne
; (în care fiecare logodnă este o pereche cu punct între parteneri)
; și o persoană, și, în cazul în care această persoană apare pe prima
; poziție într-o logodnă, este întors partenerul său, altfel se
; întoarce false.
; Folosește cond.
(define (get-partner engagements person)
  (cond
    [(null? engagements) #f]
    [(equal? person (caar engagements)) (cdar engagements)]
    [else (get-partner (cdr engagements) person)]))


; Funcție care primește 2 persoane logodite, p1 și p2,
; lista preferințelor lui p1, lista preferințelor tuturor persoanelor
; de același gen cu p2, respectiv lista tuturor logodnelor, și întoarce
; true dacă există un partener mai potrivit pentru p1, și false altfel.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - lista logodnelor este completă, este un posibil rezultat al problemei
; - logodnele din listă au pe prima poziție persoana de același gen cu p2
; - un partener p' este mai potrivit decât p2 dacă îndeplinește 2 condiții:
;   - p1 îl preferă pe p' în raport cu p2
;   - p' îl preferă pe p1 în raport cu persoana cu care este logodit
(define (better-match-exists? p1 p2 p1-list pref2 engagements)
  (cond
    [(equal? (car p1-list) p2) #f]
    [(preferable? (get-pref-list pref2 (car p1-list)) p1 (get-partner engagements (car p1-list))) #t]
    [else (better-match-exists? p1 p2 (cdr p1-list) pref2 engagements)]))
