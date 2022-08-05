#lang remora/dynamic
#;
(def (1d-conv-layer ())
  ...)


(reduce-n + 0 [1 2 3] 1)

(def (get-x-by-x-with-offset (arr 2) (x 0) (offset-x 0) (offset-y 0))
  (def y-cut-arr (take x (drop offset-y arr)))
  (#r(0 1)take x (#r(0 1)drop offset-x y-cut-arr)))

(def (apply-single-kernel (input 2) (kernel 2))
  (reduce + 0 (reduce + 0 (* input kernel))))

(def (2d-conv-single-filter (kernel 2) (input 2) (stride 0))
  (def kernel-size (length kernel))
  (def indices-unfiltered (iota [(add1 (- (length input) kernel-size))]))
  (def indices (filter (zero? (modulo indices-unfiltered stride)) indices-unfiltered))
  (def kernel-inputs (#r(2 0 1 0)get-x-by-x-with-offset input kernel-size indices indices))
  (apply-single-kernel kernel-inputs kernel))

(def (2d-conv-layer (filter-kernel 3) (input 2) (stride 0) (pad 0) (b 0))
  (+ b (2d-conv-single-filter filter-kernel (tensor-pad input pad) stride)))


(def (tensor-pad (x all) (pad 0))
  ((select
    (equal? pad 0)
    (fn () x)
    (fn () 
      (def shape (shape-of x))
      (def new-dim-shape (drop 1 (+ (* 2 pad) shape)))
      (def new-dim-template (build-array (fn ((x 0)) 0) new-dim-shape))
      (def new-dim (build-array (fn ((x 0)) new-dim-template) [pad]))
      ((select
        (equal? (length shape) 1)
        (fn () (append new-dim (append x new-dim)))
        (fn () (append new-dim (append (#r(1 0)tensor-pad x pad) new-dim)))))))))

(tensor-pad (iota [3 3]) 1)
   
; https://towardsdatascience.com/backpropagation-in-a-convolutional-layer-24c8d64d8509

(def (2d-convolution-backpropagation (dy 2) (x 2) (w 2) (b 0) (pad 0) (stride 0))
  (define w-shape (shape-of w))
  (define pad-x (tensor-pad x pad))
  (def pad-x-size (length pad-x))
  (def w-size (length w))
  (def y-size (length dy))
  (def indices-unfiltered (iota [(add1 (- pad-x-size y-size))]))
  (def indices (filter (zero? (modulo indices-unfiltered stride)) indices-unfiltered))
  (def xp-inputs (#r(2 0 1 0)get-x-by-x-with-offset pad-x y-size indices indices)) ; this hacky rerank does cartesian product over indices
  (def foo (#r(2 2)* xp-inputs dy))
  (def dw (reduce + 0 (reduce + 0 foo)))
  (def pad-dy (tensor-pad dy (sub1 w-size)))
  (def pad-dy-size (length pad-dy))
  (def pad-dx (tensor-pad (build-array (fn ((x 0)) 0) (shape-of x)) pad pad))
  (def pad-dx-size (length pad-dx))
  (def w_ (reverse (#r(1)reverse w))) ; reverse each row and then reverse order of rows
  (def pad-dy-indices (iota [(- pad-dy-size w-size -1)]))
  (show pad-dy-indices)
  (def pad-dy-inputs (#r(2 0 1 0)get-x-by-x-with-offset pad-dy w-size pad-dy-indices pad-dy-indices))
  (def bar (#r(2 2)* pad-dy-inputs w_))
  (def dxp (#r(0 0 1)reduce + 0 (#r(0 0 1)reduce + 0 bar)))
  (def dx (get-x-by-x-with-offset dxp (length x) pad pad))
  (def db (reduce + 0 (reduce + 0 dy)))
  (values dx db dw))




; a hack to make layers rank-polymorphic
(struct data (d))
(struct layer-class (layer forward-f backward-f))

; Fully connected layer
(struct fc-layer (weights bias))
; TODO: not sure how fully connected layer looks like with 3d data for example
(def (fc-layer-forward (input all) (weights all) (bias all)) 1)

(def (fc-layer-backward) 1)

(def (make-fc-layer (weights all) (bias 0))
  (layer-class (fc-layer weights bias) fc-layer-forward fc-layer-backward))


(def (get-windows (input all) (window-shape 1) (stride 0))
  (def input-shape (shape-of input))
  (def (calc-size (n 0) (window-size 0)) (add1 (floor (/ (- n window-size) stride))))
  (def output-shape (calc-size input-shape window-shape))
  (def output-indices (* stride (iota (#r(0)itemize output-shape))))
  (def all-output-window-offsets (cartesian-product output-indices))
  (def all-windows (slice input all-output-window-offsets window-shape))
  (reshape (append output-shape window-shape) all-windows))


(def (mean (x all))
  (/ (reduce-n + 0 x (length (shape-of x)))
     (reduce * 1 (shape-of x))))

(def (variance (x all) (mean 0))
  (/ (reduce-n + 0 (expt (- x mean) 2) (length (shape-of x))) (sub1 (reduce * 1 (shape-of x)))))

; Convolutional layer
; weights are n+1 dims, n is number of dimensions in data, extra dim is for multiple filters
; no bias because YOLO does not use it (it uses batch norm layer in all conv layers instead)
(struct conv-layer (weights stride pad))

; from darknet - convolutional_layer.c
#|
    // for yolo groups are always 1
    int m = l.n;
    int k = l.size*l.size*l.c;
    int n = l.out_w*l.out_h;
            float *a = l.weights;
            float *b = net.workspace;
            float *c = l.output;
            float *im =  net.input;

            if (l.size == 1) {
                b = im;
            } else {
                im2col_cpu(im, l.c, l.h, l.w, l.size, l.stride, l.pad, b);
            }
            gemm(0,0,m,n,k,1,a,k,b,n,1,c,n);



M - number of filter in a layer
N - output size (w * h for 2d)
K - input size (w * h * channels for 2d)
A - weights
lda - input size

B - input
ldb - also output img size?

C - output
ldc - output img size

for each filter with index i:
  for each pixel in input with index k:
    for each pixel in output with index j:
      
void gemm_nn_simple(int M, int N, int K,
        float *weights, int lda, 
        float *input, int ldb,
        float *output, int ldc)
{
    int i,j,k;
    for(i = 0; i < M; ++i){
        for(k = 0; k < K; ++k){
            for(j = 0; j < N; ++j){
                output[i*ldc+j] += weights[i*lda+k]*input[k*ldb+j];
            }
        }
    }
}
|#
; returns a output size for a dimenstion given input size n and filter size w-size
(def (conv-output-size (n 0) (w-size 0) (pad 0) (stride 0))
  (add1 (floor (/ (+ n (* 2 pad) (- w-size)) stride))))

; layer is the conv-layer struct, input is the data struct
(def (conv-layer-forward (w all) (pad 0) (stride 0) (input all))
  (define w-shape (shape-of w))
  (define padded-input (tensor-pad input pad))
  (def all-windows (get-windows padded-input w-shape stride))
  (reduce-n + 0 (* w all-windows) (length w-shape)))

(conv-layer-forward (build-array (fn ((x 0)) 1) [2 2]) 0 1 (iota [3 3]))

; w adn dy are a single filter/delta combo, and applications are nice because w's adn dy's agree in the top dimension
(def (conv-layer-backward (dy all) (w all) (input all) (pad 0) (stride 0))
  (define w-shape (shape-of w))
  (define dy-shape (shape-of dy))
  (define padded-input (tensor-pad input pad))
  (def all-input-windows (get-windows padded-input dy-shape stride))
  (def dw (reduce-n + 0 (* all-input-windows dy) (length (shape-of dy))))
  (def pad-dy (tensor-pad dy (sub1 (index w-shape 0))))
  (def pad-dy-size (length pad-dy))
  (def pad-dy-inputs (get-windows pad-dy w-shape stride))
  (def unfolded-dxp (* pad-dy-inputs w))
  (def dxp (reduce-n + 0 unfolded-dxp (length w-shape)))
  (values dw dxp))

(def (make-conv-layer (filter-size 1) (filter-num 0) (stride 0) (pad 0))
  (layer-class (conv-layer (error) stride pad) conv-layer-forward conv-layer-backward))

; Activation layer
(struct act-layer (act-f act-f-prime))

(def (act-layer-forward (layer 0) (input all))
  (def f (act-layer-act-f layer))
  (f input))
(def (act-layer-backward (layer 0) (dy all))
  (def f-prime (act-layer-act-f-prime layer))
  (* (f-prime dy) dy))

(def (make-act-layer) 1)

; Actiovation function + derivatives
(def (leaky-relu (x 0))
  (select (> x 0) x (* 0.1 x)))

(def (leaky-relu-prime (x 0))
  (select (> x 0) 1 0.1))

(def (linear (x 0)) x)

(def (linear-prime (x 0)) 1)

; Max pooling layer
(struct max-pool (size))

; input is single input data 'frame'
(def (max-pool-layer-forward (input all) (stride 0) (size 0) (pad 0))
  (def padded-input (tensor-pad input pad))
  (def padded-input-shape (shape-of padded-input))
  (def pool-shape (build-array (fn ((_ 0)) size) [(length padded-input-shape)]))
  (def windows (get-windows padded-input pool-shape stride))
  
  (reduce-n max -inf.0 windows (length padded-input-shape)))
;(max-pool-layer-forward (iota [4 4]) 2 2 1)

(def (max-pool-layer-backward) 1)

; Dropout layer
(struct dropout (prob))

; rand-number-gen for consistent randomness during debugging
(def (dropout-layer-forward (input all) (prob 0) (scale 0) (rand-number-gen 0))
  (def input-shape (shape-of input))
  (def random-numbers (build-array (fn ((_ 0)) (random rand-number-gen)) input-shape))
  (def dropout-filter (< random-numbers prob))
  (def dropout-select (fn ((bool 0) (x 0)) (select bool 0 x)))
  (def dropout-result (dropout-select dropout-filter input))
  (values dropout-result dropout-filter))
(def-values (d-res d-fil) (dropout-layer-forward (iota [4 4]) 0.5 1 (current-pseudo-random-generator)))

(def (dropout-layer-backward (input-filter all) (dy all) (scale 0))
  (def dropout-back (fn ((bool 0) (x 0)) (select bool 0 (* x scale))))
  (dropout-back input-filter dy))
(println "foo")
(show d-fil)
(show d-res)
(dropout-layer-backward d-fil (build-array (fn ((_ 0)) (random)) [4 4]) 1)

(struct detection (foo))

; Batch-normalization layer

(struct batch-norm (rolling-mean rolling-var))

(def (batch-norm-forward) (error))

(def (batch-norm-backward) (error))

; layers is [gen-layer]
(struct network (layers input output))






































