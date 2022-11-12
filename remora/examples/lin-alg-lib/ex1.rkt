#lang remora/dynamic

(def (dot-product (x 1) (y 1))
  (reduce + 0 (* x y)))

(dot-product [1 2] [3 4])

(def (saxpy (y 1) (x 1) (a 0))
  (+ y (* x a)))

(saxpy [1 1 1] [1 2 3] 2)

(def (gaxpy (y 1) (A 2) (x 1))
  (+ y (reduce + 0 (* A x))))

  
(gaxpy [1 0] [[1 2] [3 4]] [1 1])

(gaxpy [[0 0] [0 0]] [[1 2] [3 4]] [[5 6] [7 8]])
