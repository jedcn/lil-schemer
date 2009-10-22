;; The Little Schemer
;;
;; Reading the book, following along with emacs, mzscheme, and Ubuntu.

;; run-scheme
;; C-x C-e Send last sexpr to Scheme
;; C-c C-r Send region

;;
;; (atom? sexp)
;;
;; Determine whether a sexp is an atom or not
;;
;; Chapter 1, p. 10
;;
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;;
;; (lat? list)
;;
;; Determine whether a list is a list containing only atoms
;;
;; Chapter 2, p 16
;;
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))
;;(lat? (quote (a b c)))
;;(lat? (quote ((a b) 1 2)))

;;
;; (member? a lat)
;;
;; Determine whether an atom, a, is present in the list of atoms, lat
;;
;; Chapter 2, p 16
;;
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? a (car lat)) #t)
      (else
       (member? a (cdr lat))))))
;;(member? 'a '(a b c))
;;(member? 'b '(a b c))
;;(member? 'c '(a b c))
;;(member? 'd '(a b c))

;;
;; (rember a lat)
;;
;; Return a list of atoms that matches lat, except without the first occurrence
;; of the atom a in lat
;;
;; Chapter 3, p. 34
;;
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

;;
;; (firsts lol)
;;
;; Given a list containing other lists, lol, return the first element
;; of each list in lol, a list of lists.
;;
;; Chapter 3, p. 44
;;
(define firsts
  (lambda (lol)
    (cond
      ((null? lol) (quote()))
      (else
       (cons (car (car lol)) (firsts (cdr lol)))))))
;;(firsts (quote((a b) (c d) (e f))))
;;(firsts (quote((a b) (c d e) (f) (g) (h i j))))

;;
;; (insertR new old lat)
;;
;; Given a list of atoms lat, a new atom, new, and an old atom, old,
;; return a new list of atoms that matches lat except with new
;; inserted to the right of the first occurrence of old
;;
;; Chapter 3, p. 47
;;
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old) (cons old (cons new (cdr lat))))
      (else
       (cons (car lat) (insertR new old (cdr lat)))))))
;;(insertR (quote jalapeno) (quote and) (quote (tacos tamales and salsa)))

;;
;; (insertL new old lat)
;;
;; Given a list of atoms lat, a new atom, new, and an old atom, old,
;; return a new list of atoms that matches lat except with new
;; inserted to the left of the first occurrence of old
;;
;; Chapter 3, p. 51
;;
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons new lat))
      (else
       (cons (car lat) (insertL new old (cdr lat)))))))
;;(insertL (quote a) (quote b) (quote (b c d)))
;;(insertL (quote a) (quote d) (quote (b c d)))

;;
;; (subst new old lat)
;;
;; Given a list of atoms lat, a new atom, new, and an old atom, old,
;; return a new list of atoms that matches lat except with new
;; having replaced first occurrence of old
;;
;; Chapter 3, p 51
;;
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else
       (cons (car lat) (subst new old (cdr lat)))))))
;;(subst (quote new) (quote old) (quote (a b c old)))
;;(subst (quote new) (quote old) (quote (old a b c)))

;;
;; (subst2 new o1 o2 lat)
;;
;; Given a list of atoms lat, a new atom, new, and two old atoms, o1
;; and o2, return a new list of atoms that matches lat except with new
;; having replaced first occurrence of either o1 or o2.
;;
;; Return the list of atoms, lat, with new having replaced either the first
;; occurrence of o1 or o2
;;
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
      ((zero? n) #f)
      ((zero? m) #t)
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
      ((zero? m) #f)
      ((zero? n) #t)
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
      ((o> n m) #f)
      ((o< n m) #f)
      (else
       #t))))

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
n     (else
      (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

;;(rempick 1 (quote (a b c d)))
;;(rempick 2 (quote (a b c d)))
;;(rempick 3 (quote (a b c d)))
;;(rempick 4 (quote (a b c d)))

;; (no-nums lat)
;;
;; Given a list of atoms, lat, return a new list of atoms based on lat
;; with all of the atoms removed.
;;
;; Chapter 4, p. 77
;;
(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     ((number? (car lat)) (no-nums (cdr lat)))
     (else
      (cons (car lat) (no-nums (cdr lat)))))))

;;(no-nums (quote (a 1 b 2 c 3 d 4 e 5)))
;;(no-nums (quote (a b c d e)))
;;(no-nums (quote (1 2 3 4 5)))

;; (all-nums lat)
;;
;; Given a list of atoms, lat, return a tuple containing all of the
;; numbers in lat
;;
;; Chapter 4, p. 77
;;
(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
     (else
      (all-nums (cdr lat))))))

(all-nums (quote (a 1 b 2 c 3 d 4 e 5)))
(all-nums (quote (a b c d e)))
(all-nums (quote (1 2 3 4 5)))

;; (eqan? a1 a2)
;;
;; Given two atoms (numeric or otherwise) return #t if they are
;; equal, #f otherwise.
;;
;; Similar to =, but works with strings
;; Similar to eq?, but works with numbers.
;;
;; Chapter 4, p. 78
;;
(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2)) (= a1 a2))
     ((or (number? a1) (number? a2)) #f)
     (else
      (eq? a1 a2)))))

;;(eqan? 1 1)
;;(eqan? 1 2)
;;(eqan? (quote a) (quote a))
;;(eqan? (quote a) (quote b))
;;(eqan? (quote a) 1)

;; (occur a lat)
;;
;; Given an atom, a, and a list of atoms, lat, return the number of
;; times that a occurs in lat.
;;
;; Chapter 4, p. 78
;;
(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
     (else
      (occur a (cdr lat))))))

;;(occur 1 (quote (a 1 b 1 b 1 c)
;;(occur (quote b) (quote (a 1 b 1 b 1 c)))

;; (one? n)
;;
;; Returns #t if n is the number 1
;;
;; Chapter 4, p. 79
;;
(define one?
  (lambda (n)
    (cond
     ((zero? n) #f)
     ((zero? (sub1 n)) #t)
     (else #f))))

;;(one? 0)
;;(one? 1)
;;(one? 2)

;; (rempick n lat)
;;
;; rempick, but using one?
;;
;; Chapter 4, p. 79
;;
(define rempick
  (lambda (n lat)
    (cond
     ((null? lat) (quote ()))
     ((one? n) (cdr lat))
     (else
      (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

;;(rempick 1 (quote (a b c d)))
;;(rempick 2 (quote (a b c d)))
;;(rempick 3 (quote (a b c d)))
;;(rempick 4 (quote (a b c d)))

;; (rember* a l)
;;Chapter 5, p. 81
(define rember*
  (lambda (a l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eqan? a (car l)) (rember* a (cdr l)))
       (else
        (cons (car l) (rember* a (cdr l))))))
     (else
      (cons (rember* a (car l)) (rember* a (cdr l)))))))

;;(rember* (quote a) (quote ((a b) (a b))))
;;(rember* (quote a) (quote ((a b))))
;;(rember* (quote a) (quote (a b c (a b))))
;;(rember* (quote a) (quote (b a b)))

;; (insertR* new old l)
;; Chapter 5, p. 82

;; (occur* a l)
;; Chapter 5, p. 85
;; 
;; (subst* new old l)
;; Chapter 5, p. 85

;; (insertL* new old l)
;; Chapter 5, p. 86

;; (member* a l)
;; Chapter 5, p. 87

;; (leftmost* l)
;; Chapter 5, p. 88

;; (eqlist? l1 l2)
;; use eqan?
;; Chapter 5, p. 92

;; (equal? s1 s2)
;; Chapter 5, p. 92

;; (rember s l)
;; Chapter 5, p. 94