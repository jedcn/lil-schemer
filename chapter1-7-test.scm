;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname chapter1-7-test) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(require scheme/include)
(include "chapter1-7.scm")

(lat? (quote (a b c)))
(lat? (quote ((a b) 1 2)))

(member? 'a '(a b c))
(member? 'b '(a b c))
(member? 'c '(a b c))
(member? 'd '(a b c))

(rember 'a '(a b c))
(rember 'b '(a b c))
(rember 'c '(a b c))
(rember 'd '(a b c))

(insertR (quote jalapeno) (quote and) (quote (tacos tamales and salsa)))

(insertL (quote a) (quote b) (quote (b c d)))
(insertL (quote a) (quote d) (quote (b c d)))

(subst (quote new) (quote old) (quote (a b c old)))
(subst (quote new) (quote old) (quote (old a b c)))

(subst2 (quote clean) (quote g1) (quote g2) (quote (a b c g2)))

(multirember (quote a) (quote (a b c)))
(multirember (quote c) (quote (a b c)))
(multirember (quote a) (quote (a b c a)))

(multiinsertR (quote z) (quote a) (quote (a a a a)))

(multiinsertL (quote z) (quote a) (quote (a a a a)))
(multisubst (quote a) (quote b) (quote (a b c d)))
(multisubst (quote a) (quote b) (quote (a a a a)))
(o+ 1 1)
(o+ 5 4)

(o- 10 5)
(o- 10 1)
(addtup (quote (1 2 3)))
(addtup (quote (10 15 5)))
(ox 6 0)
(ox 6 1)
(ox 6 3)
(ox 6 10)
(tup+ (quote (1 2)) (quote (3 5)))
(tup+ (quote (5 5 5 5 56)) (quote (4 27 8 9)))

(o> 3 2)
(o> 2 3)
(o> 2 2)
(o< 3 2)
(o< 2 3)
(o< 2 2)
(o= 1 2)
(o= 2 2)
(o^ 2 0)
(o^ 2 3)
(o^ 2 5)
(olength (quote(a b c)))
(pick 3 (quote (a b c)))
(pick 1 (quote (a b c)))

(rempick 1 (quote (a b c d)))
(rempick 2 (quote (a b c d)))
(rempick 3 (quote (a b c d)))
(rempick 4 (quote (a b c d)))
(no-nums (quote (a 1 b 2 c 3 d 4 e 5)))
(no-nums (quote (a b c d e)))
(no-nums (quote (1 2 3 4 5)))
(all-nums (quote (a 1 b 2 c 3 d 4 e 5)))
(all-nums (quote (a b c d e)))
(all-nums (quote (1 2 3 4 5)))
(eqan? 1 1)
(eqan? 1 2)
(eqan? (quote a) (quote a))
(eqan? (quote a) (quote b))
(eqan? (quote a) 1)

(occur 1 (quote (a 1 b 1 b 1 c)))
(occur (quote b) (quote (a 1 b 1 b 1 c)))
(one? 0)
(one? 1)
(one? 2)
(rempick-again 1 (quote (a b c d)))
(rempick-again 2 (quote (a b c d)))
(rempick-again 3 (quote (a b c d)))
(rempick-again 4 (quote (a b c d)))

(eqlist? (quote ()) (quote ()))
(eqlist? (quote ()) (quote (a)))
(eqlist? (quote (a)) (quote (a)))
(eqlist? (quote (a)) (quote (b)))
(eqlist? (quote (a b)) (quote (a b)))
(eqlist? (quote (a (b))) (quote (a (c))))

(equal (quote ()) (quote ()))
(equal (quote a) (quote a))
(equal (quote a) (quote b))
(equal (quote ()) (quote a))
(equal (quote (a b)) (quote (a b)))
(equal (quote (a b)) (quote (a b c)))
(equal (quote (a b (c d) (e f) g)) (quote (a b (c d) (e f) g)))
(equal (quote (a b (c d) (e f (r r)) () g)) (quote (a b (c d) (e f (r r)) () g)))

(numbered? (quote 4))
(numbered? (quote (4 + 5)))
(numbered? (quote ((3 + 1) + 5)))
(numbered? (quote ((3 ^ 2) + (5 - 2))))

(1st-sub (quote (4 + 5)))
(1st-sub (quote ((4 ^ 3) + 5)))

(2nd-sub (quote (4 + 5)))
(2nd-sub (quote ((4 ^ 3) + 5)))
(2nd-sub (quote ((4 ^ 3) + (5 - 2))))

(operator (quote (4 + 5)))
(operator (quote ((4 ^ 3) ^ 5)))
(operator (quote ((4 ^ 3) - (5 - 2))))

(value (quote 1))
(value (quote (1 + 3)))
(value (quote (3 - 1)))
(value (quote (2 ^ 3)))
(value (quote (1 + (3 ^ 4))))
(value (quote ((1 + (3 ^ 4)) - 2)))

(set? (quote ()))
(set? (quote (a)))
(set? (quote (a a)))
(set? (quote (a b c)))
(set? (quote (a b c d e f g h i j k f)))

(makeset (quote ()))
(makeset (quote (a)))
(makeset (quote (a a)))
(makeset (quote (a b c)))
(makeset (quote (a b c d e f g h i j k f)))
(makeset (quote (a b a b a b a b a)))

(subset? (quote ()) (quote (a b)))
(subset? (quote (a)) (quote (a)))
(subset? (quote (a b c)) (quote (a b)))
(subset? (quote (a b c d)) (quote (d a b c)))

(eqset? (quote ()) (quote (a b)))
(eqset? (quote (a)) (quote (a)))
(eqset? (quote (a b c)) (quote (a b)))
(eqset? (quote (a b c d)) (quote (d a b c)))

(intersect? (quote ()) (quote (a b)))
(intersect? (quote (a)) (quote (a)))
(intersect? (quote (a b c)) (quote (d e f)))
(intersect? (quote (a b c d)) (quote (d e f g)))

(intersect (quote ()) (quote (a b)))
(intersect (quote (a)) (quote (a)))
(intersect (quote (a b c)) (quote (b d e f)))
(intersect (quote (a b c d)) (quote (d c e f g)))

(union (quote ()) (quote (a b)))
(union (quote (a)) (quote (a)))
(union (quote (a b c)) (quote (b d e f)))
(union (quote (a b c d)) (quote (d c e f g)))

(intersect-all (quote ((a b c) (a b c d e f) (b c f g))))