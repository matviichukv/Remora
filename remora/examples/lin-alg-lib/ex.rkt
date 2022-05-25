#lang remora/dynamic

(def (dot-product (a 1) (b 1))
  (reduce + 0 (* a b)))

(def (transpose-vec (a 1))
  ((fn ((i 0)) [i]) a))

(define (get-col (A 2) (i 0)) (#r(1 1)index A [i]))

(def (transpose (A 2))
  (define shape (shape-of A))
  (define col-len (head shape))
  (define new-idxs (iota [col-len]))
  (get-col A new-idxs))

(def (matrix-mult (A 1) (B 2))
  (dot-product A (transpose B)))

; to produce a matrix
(def (vector-mult (a 1) (b 1))
  (#r(2) ravel (#r(0 1)* (transpose-vec a) b)))
#;
(vector-mult [1 2 3] [4 5])

(def (symmetric? (A 2))
  (equal (transpose A) A))
#;
(symmetric? (iota [3 3]))


(def (identity-mat (n 0))
  (define (gen-row (i 0))
    (build-array (fn ((j 0)) (bool->int (equal i j))) [n]))
  (gen-row (iota [n])))
#;
(identity-mat 5)
#;
(symmetric? (identity-mat 5))




