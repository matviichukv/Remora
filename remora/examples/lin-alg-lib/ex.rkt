#lang remora/dynamic

(def (println (a all))
  (show a)
  (display "\n"))

(def (forall (a 1) (f 0))
  (define shape (shape-of a))
  (reduce and #t (f #:result-shape shape a)))

(def (dot-product (a 1) (b 1))
  (reduce + 0 (* a b)))

(def (transpose-vec (a 1))
  (#r(0)itemize a))

#;
(transpose-vec [1 2 3])

(define (get-col (A 2) (i 0)) (#r(1 1)index A [i]))

(def (transpose (A 2))
  (define shape (shape-of A))
  (define row-len (index shape [1]))
  (define new-idxs (iota [row-len]))
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

(def (skew-symmetric? (A 2))
  (equal (transpose A) (- A)))


(def (identity-mat (n 0))
  (define (gen-row (i 0))
    (build-array (fn ((j 0)) (bool->int (equal i j))) [n]))
  (gen-row (iota [n])))
#;
(identity-mat 5)
#;
(symmetric? (identity-mat 5))

(def (col-lower-bounded? (col 1) (j 0) (p 0))
    (define rest (drop (min (length col) (+ j p 1)) col))
    (forall rest (fn ((n 0)) (equal n 0))))

(def (lower-bound? (A 2) (col-idxs 1) (p 0))
  (reduce and #t (col-lower-bounded? (transpose A) col-idxs p)))

(def (find-lower-bound (A 2))
  (define shape (shape-of A))
  (define row-len (index shape [1]))
  (define col-len (index shape [0]))
  (define p-vals (iota [col-len]))
  (define col-idxs (iota [row-len]))
  (define valid-p-vals (filter* (lower-bound? A col-idxs p-vals) p-vals))
  (define unbox-valid-p-vals (unsafe-unbox valid-p-vals))
  (reduce min col-len unbox-valid-p-vals))

#;
(find-lower-bound (identity-mat 4)) ; return 0
#;
(find-lower-bound (iota [4 4])) ; return 3

(def (row-upper-bounded? (row 1) (i 0) (q 0))
    (define rest (drop (min (length row) (+ i q 1)) row))
    (forall rest (fn ((n 0)) (equal n 0))))

(def (upper-bound? (A 2) (row-idxs 1) (q 0))
  (reduce and #t (row-upper-bounded? A row-idxs q)))

(def (find-upper-bound (A 2))
  (define shape (shape-of A))
  (define row-len (index shape [1]))
  (define col-len (index shape [0]))
  (define q-vals (iota [row-len]))
  (define row-idxs (iota [col-len]))
  (define valid-q-vals (filter* (upper-bound? A row-idxs q-vals) q-vals))
  (define unbox-valid-q-vals (unsafe-unbox valid-q-vals))
  (reduce min col-len unbox-valid-q-vals))

#;
(find-upper-bound (identity-mat 4)) ; return 0
#;
(find-upper-bound (iota [4 4])) ; return 3

(def (get-matrix-band (A 2))
  (def lower-bound (find-lower-bound A))
  (def upper-bound (find-upper-bound A))
  [lower-bound upper-bound])

#;
(get-matrix-band [[1 2 3 0]
                  [0 4 5 6]
                  [0 0 7 8]
                  [0 0 0 0]])

(def (is-matrix-banded? (A 2))
  (def col-len (index (shape-of A) [0]))
  (def row-len (index (shape-of A) [1]))
  (def lower-bound (find-lower-bound A))
  (def upper-bound (find-upper-bound A))
  (and (< lower-bound col-len) (< upper-bound row-len) ))

#;
(is-matrix-banded? (identity-mat 4)) ; true
#;
(is-matrix-banded? (iota [4 4])) ; false

(def (compact-band-matrix (A 2))
  (def lower-bound (find-lower-bound A))
  (def upper-bound (find-upper-bound A))
  (def col-len (index (shape-of A) [0]))
  (def row-len (index (shape-of A) [1])) 
  1)

(def (parallel-add (a 1) (b 1) (base 0))
  (def (add-with-carry (prev 0) (cur 0))
    (+ cur (quotient prev base)))
  (define res-with-carry (reverse (behead (scan add-with-carry 0 (reverse (+ a b))))))
  (modulo (append [(quotient (head res-with-carry) base)] res-with-carry) base))

(parallel-add [7 8 9] [4 5 6] 10)

(def (hacky-parallel-add (a 1) (b 1) (base 0))
  (def (add-with-carry (prev 0) (cur 0))
    (+ cur (quotient prev base)))
  (define res-with-carry (scan-weird add-with-carry 0 (reverse (+ a b))))
  (define head-element (quotient (index res-with-carry [1]) base))
  (modulo (array-set res-with-carry [0] head-element) base))
  

(hacky-parallel-add [7 8 9] [4 5 6] 10)
(hacky-parallel-add [9] [6] 10)

 