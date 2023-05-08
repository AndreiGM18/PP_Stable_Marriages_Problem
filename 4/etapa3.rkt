; Mitran Andrei-Gabriel, 323CA, 2023

#lang racket

(require "etapa2.rkt")

(provide (all-defined-out))

; După modelul funcției stable-match?, este implementată funcția
; get-unstable-couples care primește o listă de logodne
; engagements, o listă de preferințe masculine mpref și o 
; listă de preferințe feminine wpref, și întoarce lista
; tuturor cuplurilor instabile din engagements.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; Nu este permisă recursivitatea pe stivă.
; Nu sunt permise alte funcții ajutătoare decât
; better-match-exists? și funcțiile de manipulare a listelor de
; preferințe definite în etapele anterioare.
; Nu este permisă apelarea multiplă a aceleiași funcții pe
; aceleași argumente.
; Folosește una sau mai multe dintre expresiile let, let*, letrec,
; named let pentru a se putea conforma acestor restricții.
(define (get-unstable-couples engagements mpref wpref)
  (let ([rev-engagements (map (λ (pair) (cons (cdr pair) (car pair))) engagements)])
    (let helper-acc ([eng engagements] [acc '()])
      (if (null? eng) acc
          (let* ([pair (car eng)] [woman (car pair)] [man (cdr pair)] [curr-wpref (get-pref-list wpref woman)] [curr-mpref (get-pref-list mpref man)])
            (helper-acc (cdr eng)
                        (if (or (better-match-exists? woman man curr-wpref mpref rev-engagements) (better-match-exists? man woman curr-mpref wpref engagements))
                            (cons pair acc) acc)
                        ))))))


; Este implementată funcția engage care primește o listă free-men
; de bărbați încă nelogodiți, o listă de logodne parțiale 
; engagements (unde fiecare cuplu are pe prima poziție o femeie),
; o listă de preferințe masculine mpref și o listă de preferințe 
; feminine wpref, și întoarce o listă completă de logodne stabile,
; obținută conform algoritmului Gale-Shapley:
; - cât timp există un bărbat m încă nelogodit
;   - w = prima femeie din preferințele lui m pe care m nu a cerut-o încă
;   - dacă w este nelogodită, adaugă perechea (w, m) la engagements
;   - dacă w este logodită cu m'
;     - dacă w îl preferă pe m lui m'
;       - m' devine liber
;       - actualizează partenerul lui w la m în engagements
;     - altfel, repetă procesul cu următoarea femeie din lista lui m
; Folosește named let pentru orice proces recursiv ajutător (deci nu
; vor fi definite funcții ajutătoare recursive).
; Folosește let și/sau let* pentru a evita calcule duplicate.
(define (engage free-men engagements mpref wpref)
  (let engage-helper ([single-men free-men] [eng engagements])
    (if (null? single-men) eng
        (let* ([man (car single-men)] [curr-mpref (get-pref-list mpref man)])
          (let find-perfect-match ([curr-mpref-iter curr-mpref])
            (let* ([woman (car curr-mpref-iter)] [curr-wpref (get-pref-list wpref woman)] [curr-partner (get-partner eng woman)])
              (cond
                [(not curr-partner) (engage-helper (cdr single-men) (cons (cons woman man) eng))]
                [(preferable? curr-wpref man curr-partner) (engage-helper (cons curr-partner (cdr single-men)) (update-engagements eng woman man))]
                [else (find-perfect-match (cdr curr-mpref-iter))])))))))


; Este implementată funcția gale-shapley care este un wrapper pentru
; algoritmul implementat de funcția engage. Funcția gale-shapley
; primește doar o listă de preferințe masculine mpref și o listă
; de preferințe feminine wpref și calculează o listă completă de
; logodne stabile conform acestor preferințe.
(define (gale-shapley mpref wpref)
  (engage (get-men mpref) '() mpref wpref))


; Este implementată funcția get-couple-members care primește o listă
; de perechi cu punct și întoarce o listă simplă cu toate elementele 
; care apar în perechi.
; Folosește funcționale, fără recursivitate explicită.
(define (get-couple-members pair-list)
  (foldr (λ (pair acc) (cons (cdr pair) (cons (car pair) acc))) '() pair-list))

