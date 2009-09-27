;; The Little Schemer

;; atom?
;;
;; Determine whether a sexp is an atom or not
;; Chapter 1, p. 10
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; lat?
;;
;; Determine whether a sexp is a list containing only atoms
;; Chapter 2, p 16
(define lat?
  (lambda (l)
    (cond
      ((null? l) true)
      ((atom? (car l)) (lat? (cdr l)))
      (else false))))

;; member?
;;
;; Determine whether the atom a is present in the list of atoms lat
;; Chapter 2, p 16
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) false)
      ((eq? a (car lat)) true)
      (else
       (member? a (cdr lat))))))
;;(member? 'a '(a b c))
;;(member? 'b '(a b c))
;;(member? 'c '(a b c))
;;(member? 'd '(a b c))

;; rember
;; Chapter 3, p. 34
;; Return a list of atoms that matches lat, except without the first occurrence
;; of the atom a in lat
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      ((eq? a (car lat)) (cdr lat))
      (else
       (cons (car lat) (rember a (cdr lat)))))))

;;(rember 'a '(a b c))
;;(rember 'b '(a b c))
;;(rember 'c '(a b c))
;;(rember 'd '(a b c))

;; firsts
;; Return the first element of each list in lol, a list of lists.
;; Chapter 3, p. 44
(define firsts
  (lambda (lol)
    (cond
      ((null? lol) (quote()))
      (else
       (cons (car (car lol)) (firsts (cdr lol)))))))

;;(firsts (quote((a b) (c d) (e f))))
;;(firsts (quote((a b) (c d e) (f) (g) (h i j))))

;; insertR
;; Return the list of atoms, lat, with new inserted to the right of the first
;; occurrence of old
;; Chapter 3, p. 47
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old) (cons old (cons new (cdr lat))))
      (else
       (cons (car lat) (insertR new old (cdr lat)))))))
;;(insertR (quote jalapeno) (quote and) (quote (tacos tamales and salsa)))

;; insertL
;; Return the list of atoms, lat, with new inserted to the left of the first
;; occurrence of old
;; Chapter 3, p. 51 
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons new lat))
      (else
       (cons (car lat) (insertL new old (cdr lat)))))))
;;(insertL (quote a) (quote b) (quote (b c d)))
;;(insertL (quote a) (quote d) (quote (b c d)))

;; subst
;; Return the list of atoms, lat, with new having replaced the first occurrence
;; of old
;; Chapter 3, p 51
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else
       (cons (car lat) (subst new old (cdr lat)))))))

;;(subst (quote new) (quote old) (quote (a b c old)))
;;(subst (quote new) (quote old) (quote (old a b c)))

;; subst2
;; Return the list of atoms, lat, with new having replaced either the first
;; occurrence of o1 or o2
;; Chapter 3, p 52
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
      (else
       (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))
;;(subst2 (quote clean) (quote g1) (quote g2) (quote (a b c g2)))

;; multirember
;; Remove all occurrences of a within a list of atoms, lat.
;; Chapter 3, p. ??
(define multirember
  (lambda (a lat)
    (cond 
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else
       (cons (car lat) (multirember a (cdr lat)))))))

;;(multirember (quote a) (quote (a b c)))
;;(multirember (quote c) (quote (a b c)))
;;(multirember (quote a) (quote (a b c a)))

;; multiinsertR
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else
       (cons (car lat) (multiinsertR new old (cdr lat)))))))
;;(multiinsertR (quote z) (quote a) (quote (a a a a)))

;; multiinsertL
(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons new (multiinsertR new old (cdr lat))))
      (else
       (cons (car lat) (multiinsertL new old (cdr lat)))))))
;;(multiinsertL (quote z) (quote a) (quote (a a a a)))

;; multisubst
(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
      (else
       (cons (car lat) (multisubst new old (cdr lat)))))))

;;(multisubst (quote a) (quote b) (quote (a b c d)))
;;(multisubst (quote a) (quote b) (quote (a a a a)))


;; o+
;; Add two numbers together
;; Chapter 4, p. 60
(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else
       (add1 (o+ n (sub1 m)))))))

;;(o+ 1 1)
;;(o+ 5 4)

;; Subtract m from n
(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else
       (sub1 (o- n (sub1 m)))))))

;;(o- 10 5)
;;(o- 10 1)

;; Add all of the numbers within tup
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else
       (o+ (car tup) (addtup (cdr tup)))))))
;;(addtup (quote (1 2 3)))
;;(addtup (quote (10 15 5)))

;; Multipy n x m
(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else
       (o+ n (x n (sub1 m)))))))

;;(x 6 0)
;;(x 6 1)
;;(x 6 3)
;;(x 6 10)

;; Given two tuples of equal length, generate a new tuple containing each
;; pair of elements added together
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

;;(tup+ (quote (1 2)) (quote (3 5)))
;;(tup+ (quote (5 5 5 5 56)) (quote (4 27 8 9)))

;; greater than
;; p72
(define o>
  (lambda (n m)
    (cond
      ((zero? n) false)
      ((zero? m) true)
      (else
       (o> (sub1 n) (sub1 m))))))

;;(o> 3 2)
;;(o> 2 3)
;;(o> 2 2)

;; less than
;; p 73
(define o<
  (lambda (n m)
    (cond
      ((zero? m) false)
      ((zero? n) true)
      (else
       (o< (sub1 n) (sub1 m))))))
;;(o< 3 2)
;;(o< 2 3)
;;(o< 2 2)

;; numeric equality
;; p 74
(define o=
  (lambda (n m)
    (cond
      ((o> n m) false)
      ((o< n m) false)
      (else
       true))))

;;(o= 1 2)
;;(o= 2 2)

;; exponentiation n^m
(define expo
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else
       (x n (expo n (sub1 m)))))))

;;(expo 2 0)
;;(expo 2 3)
;;(expo 2 5)

;; length
;; returns the number of elements in a lat
;; p 76
(define olength
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else
       (add1 (olength (cdr lat)))))))
;;(olength (quote(a b c)))

;; (pick n lat)
;;
;; Given a number, n, and a list of atoms, lat, return the nth item in
;; the list. When n is 1, return the first item.
;;
;; Chapter 4, p. 76
;;
(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else
      (pick (sub1 n) (cdr lat))))))

;;(pick 3 (quote (a b c)))
;;(pick 1 (quote (a b c)))

;; (rempick n lat)
;;
;; Given a number, n, and a list of atoms, lat, remove the nth item in
;; the list. When n is 1, remove the first item.
;;
;; Chapter 4, p. 76 
;;
(define rempick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (cdr lat))
     (else
      (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

;;(rempick 1 (quote (a b c d)))
;;(rempick 2 (quote (a b c d)))
;;(rempick 3 (quote (a b c d)))
;;(rempick 4 (quote (a b c d)))


