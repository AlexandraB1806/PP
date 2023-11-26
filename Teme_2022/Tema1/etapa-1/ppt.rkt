#lang racket
(provide (all-defined-out))

;; Un triplet pitagoreic primitiv (TPP) este format din 
;; 3 numere naturale nenule a, b, c cu proprietățile:
;;    a^2 + b^2 = c^2
;;    a, b, c prime între ele
;;
;; TPP pot fi generate sub formă de arbore (infinit) cu
;; rădăcina (3,4,5), pe baza a 3 transformări matriciale:
;;
;;      |-1 2 2|        |1 2 2|        |1 -2 2|
;; T1 = |-2 1 2|   T2 = |2 1 2|   T3 = |2 -1 2|
;;      |-2 2 3|        |2 2 3|        |2 -2 3|
;;
;;                         (3,4,5)
;;              ______________|______________
;;             |              |              |
;;         (15,8,17)      (21,20,29)     (5,12,13)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;; (35,12,37) ..........................................
;;
;; unde:
;; (15, 8,17) = T1·(3,4,5)
;; (21,20,29) = T2·(3,4,5)
;; ( 5,12,13) = T3·(3,4,5) etc.
;;
;; În această reprezentare, TPP sunt indexate "de sus în jos",
;; respectiv "de la stânga la dreapta", rezultând ordinea:
;; (3,4,5) (15,8,17) (21,20,29) (5,12,13) (35,12,37) ... etc.

;; Reprezentăm matricile T1, T2, T3 ca liste de liste:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; Implementați o funcție care calculează produsul scalar
; a doi vectori X și Y (reprezentați ca liste).
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
; Utilizați recursivitate pe stivă.
(define (dot-product X Y)
  (cond
    ;; Daca listele X sau Y nu contin elemente, produsul lor scalar este 0.
    ((or (null? X) (null? Y)) 0)
    ;; Daca cele 2 liste au elemente, iau pe rand fiecare element al listei, aplic operatia
    ;; de inmultire intre capetele celor 2 liste si aplic recursiv functia dot-product.
    (else (+ (* (car X) (car Y)) (dot-product (cdr X) (cdr Y))))))


; Implementați o funcție care calculează produsul dintre
; o matrice M și un vector V (puneți V "pe verticală").
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
; Utilizați recursivitate pe coadă.
(define (multiply M V)
  ;; Voi inversa matricea pentru a pastra ordinea elementelor inserate.
  (multiply-helper (reverse M) V '()))

(define (multiply-helper M V result)
  (cond
    ((or (null? M) (null? V)) result)
    ;; Apelez functia anterioara dot-product pe fiecare lista din matricea M.
    ;; Folosind cons, adaug pe prima pozitie rezultatul returnat de dot-product in lista result.
    (else (multiply-helper (cdr M) V (cons (dot-product (car M) V) result)))))
    

; Implementați o funcție care primește un număr n și
; întoarce o listă numerică (unde elementele au valoarea
; 1, 2 sau 3), reprezentând secvența de transformări prin
; care se obține, plecând de la (3,4,5), al n-lea TPP
; din arbore.
; Ex: (get-transformations 8) întoarce '(2 1), adică
; al 8-lea TPP din arbore se obține din T1·T2·(3,4,5).
; Sunteți încurajați să folosiți funcții ajutătoare
; (de exemplu pentru determinarea nivelului din arbore 
; pe care se află n, sau a indexului minim/maxim de pe 
; nivelul respectiv, etc.)

;; Resturile impartirii numarului adunat cu o unitate la 3 sunt mereu 0, 1 sau 2.
;; Pentru fiecare rest in parte, am atribuit o anumita transformare:
;; - Pentru restul 0 => T1
;; - Pentru restul 1 => T2
;; - Pentru restul 2 => T3
(define (get-transformations-helper n result)
  (cond
      ((equal? n 1) result)
      ;; Transformarea T1
      ((equal? (modulo (add1 n) 3) 0) (get-transformations-helper (quotient (add1 n) 3) (cons 1 result)))
      ;; Transformarea T2
      ((equal? (modulo (add1 n) 3) 1) (get-transformations-helper (quotient (add1 n) 3) (cons 2 result)))
      ;; Transformarea T3
      (else (get-transformations-helper (quotient (add1 n) 3) (cons 3 result)))))

 (define (get-transformations n)
  (get-transformations-helper n '()))


; Implementați o funcție care primește o listă Ts de 
; tipul celei întoarsă de get-transformations, respectiv 
; un triplet de start ppt și întoarce tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Utilizați recursivitate pe coadă.

(define (apply-matrix-transformations Ts ppt)
  (cond
    ;; Daca lista de transformari nu contine elemente, se returneaza tripletul initial.
    ((null? Ts) ppt)
    ;; In caz contrar, iau pe rand fiecare element din lista de transformari
    ;; si in functie de valoarea din lista, aplic transformarea corespunzatoare,
    ;; apeland functia anterioara multiply.
    ((equal? (car Ts) 1) (apply-matrix-transformations (cdr Ts) (multiply T1 ppt)))
    ((equal? (car Ts) 2) (apply-matrix-transformations (cdr Ts) (multiply T2 ppt)))
    (else (apply-matrix-transformations (cdr Ts) (multiply T3 ppt)))))


; Implementați o funcție care calculează al n-lea TPP
; din arbore, folosind funcțiile anterioare.

;; Folosind functia (get-transformations n), voi obtine lista de transformari ce s-au aplicat
;; pentru a ajunge la al n-lea triplet.
;; Ts = (get-transformations n) iar ppt = '(3 4 5) - radacina. 
(define (get-nth-ppt-from-matrix-transformations n)
  (apply-matrix-transformations (get-transformations n) '(3 4 5)))
