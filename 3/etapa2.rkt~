; Mitran Andrei-Gabriel, 323CA, 2023

#lang racket

(provide (all-defined-out))

; ATENȚIE - Veți avea de reimplementat mai multe funcții
; din etapa 1, însă cu un alt mod de rezolvare (folosind
; funcționale sau alte funcții solicitate în enunț).
; Enunțul acestor exerciții va debuta prin "Ca în etapa 1,
; dar este interzisă recursivitatea explicită".


; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Funcție care primește lista preferințelor
; bărbaților și calculează lista bărbaților din problemă.
; Folosește orice funcțională exceptând foldl/foldr.
(define (get-men mpref)
  (map car mpref))


; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Funcție care primește lista preferințelor
; femeilor și calculează lista femeilor din problemă.
; Folosește foldl sau foldr, astfel încât să nu fie necesare
; operații de tip append sau reverse.
(define (get-women wpref)
  (foldr
   (λ (L women) (cons (car L) women))
   '()
   wpref))


; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Funcție care primește o listă de liste
; de preferințe (ori lista preferințelor bărbaților, ori a
; femeilor) și o persoană ale cărei preferințe apar în listă,
; și întoarce lista preferințelor acelei persoane.
; Se garantează că persoana apare în listă.
; Exemplu: dacă în lista wpref apare linia (ana bobo adi cos),
; (get-pref-list wpref 'ana) => '(bobo adi cos)
; Folosește minim o funcțională și minim o funcție anonimă.
(define (get-pref-list pref person)
  (cdar (filter
         (λ (L) (equal? (car L) person))
         pref)))


; Ca în etapa 1, dar este interzisă recursivitatea explicită
; și sunt permiși operatorii condiționali:
; Funcție care primește o listă de tipul
; întors la exercițiul precedent (lista preferințelor unei persoane),
; respectiv două persoane x și y care apar în lista respectivă,
; și întoarce true dacă x este mai sus decât y în topul preferințelor
; și false în caz contrar.
; Folosește funcția member.
(define (preferable? pref-list x y)
  (list?
   (member y (member x pref-list))))


; Este implementată recursiv funcționala find-first, care primește
; un predicat și o listă și întoarce primul element al listei
; care satisface predicatul, sau false dacă un asemenea element
; nu există.
; Implementarea trebuie să fie eficientă în sensul că nu trebuie
; să continue explorarea listei odată ce s-a găsit elementul.
(define (find-first p L)
  (cond
    [(null? L) #f]
    [(p (car L)) (car L)]
    [else (find-first p (cdr L))]))


; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Funcție care primește o listă de logodne
; (în care fiecare logodnă este o pereche cu punct între parteneri)
; și o persoană, și, în cazul în care această persoană apare pe prima
; poziție într-o logodnă, este întors partenerul său, altfel se
; întoarce false.
; Folosește find-first, fără să îl apelați de 2 ori (hint: define în define).
(define (get-partner engagements person)
  (define (get-pair engagements person)
    (find-first
     (λ (pair) (equal? (car pair) person))
     engagements))

  ((λ (pair)
     (if (equal? #f pair) #f (cdr pair)))
   (get-pair engagements person)))
  

; Este implementată recursiv funcționala change-first care primește
; un predicat p, o listă L și o valoare val, și întoarce o nouă 
; listă în care primul element din L care satisface predicatul p
; a fost înlocuit cu valoarea val, celelalte rămânând la fel.
; Dacă niciun element din L nu satisface predicatul, lista L
; rămâne neschimbată.
(define (change-first p L val)
  (cond
    [(null? L) L]
    [(p (car L)) (cons val (cdr L))]
    [else (cons (car L) (change-first p (cdr L) val))]))


; Este implementată funcția update-engagements care primește o listă de
; logodne engagements și două persoane p1 și p2, și întoarce lista
; actualizată de logodne, în care vechiul partener al lui p1 este
; înlocuit cu p2.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - p1 era logodită în prealabil
; - fiecare cuplu din lista engagements are pe prima poziție
;   persoanele de același gen cu p1
; Folosește change-first.
(define (update-engagements engagements p1 p2)
  (change-first
   (λ (pair) (equal? (car pair) p1))
   engagements (cons p1 p2)))


; Este copiată implementarea funcției better-match-exists? din etapa 1.
; Funcția nu este repunctată de checker, dar este necesară pentru
; implementarea funcției stable-match? de mai jos.
(define (better-match-exists? p1 p2 p1-list pref2 engagements)
  (cond
    [(equal? (car p1-list) p2) #f]
    [(preferable? (get-pref-list pref2 (car p1-list)) p1 (get-partner engagements (car p1-list))) #t]
    [else (better-match-exists? p1 p2 (cdr p1-list) pref2 engagements)]))


; Este implementată funcția stable-match? care primește o listă 
; completă de logodne engagements, o listă de preferințe masculine 
; mpref și o listă de preferințe feminine wpref, și întoarce true 
; dacă toate cuplurile din engagements sunt stabile.
; Un cuplu este stabil dacă pentru niciunul din membrii cuplului
; nu există un alt partener mai potrivit (conform definiției de
; la funcția better-match-exists?).
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
(define (stable-match? engagements mpref wpref)
  (null? (filter
          (λ (pair) (or
                     (better-match-exists? (car pair) (cdr pair) (get-pref-list wpref (car pair)) mpref engagements)
                     (better-match-exists? (cdr pair) (car pair) (get-pref-list mpref (cdr pair)) wpref engagements)))
          engagements)))

