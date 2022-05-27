#lang remora/dynamic

(def (println (a all))
  (show a)
  (display "\n"))

(def (forall (a 1) (f 0))
  (define shape (shape-of a))
  (println shape)
  (reduce and #t (f a)))

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


(def (find-lower-bound (A 2))
  (define shape (shape-of A))
  (define row-len (index shape [1]))
  (define col-len (index shape [0]))
  (define p-vals (iota [col-len]))
  (println p-vals)
  (def (col-lower-bounded? (col 1) (j 0) (p 0))
    (define rest (drop (+ j p) col))
    (println j)
    (forall rest (fn ((n 0)) (equal n 0))))
  (def (lower-bound? (p 0))
    (col-lower-bounded? (transpose A) row-len p))
  (define valid-p-vals (filter* (lower-bound? p-vals) p-vals))
  (min valid-p-vals))

(find-lower-bound (identity-mat 4))
