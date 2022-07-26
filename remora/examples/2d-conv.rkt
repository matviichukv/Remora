#lang remora/dynamic
#;
(def (1d-conv-layer ())
  ...)

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
  (def shape (shape-of x))
  (def new-dim-shape (drop 1 (+ (* 2 pad) shape)))
  (def new-dim-template (build-array (fn ((x 0)) 0) new-dim-shape))
  (def new-dim (build-array (fn ((x 0)) new-dim-template) [pad]))
  ((select
   (equal? (length shape) 1)
   (fn () (append new-dim (append x new-dim)))
   (fn () (append new-dim (append (#r(1 0)tensor-pad x pad) new-dim))))))
#;
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
  (printf "\n")
  (show pad-dy)
  (printf "\n")
  (def pad-dy-inputs (#r(2 0 1 0)get-x-by-x-with-offset pad-dy w-size pad-dy-indices pad-dy-indices))
  (show pad-dy-inputs)
  (printf "\n")
  (def bar (#r(2 2)* pad-dy-inputs w_))
  (show (shape-of bar))
  (printf "\n")
  (def dxp (#r(0 0 1)reduce + 0 (#r(0 0 1)reduce + 0 bar)))
  (show dxp)
  (printf "\n")
  (def dx (get-x-by-x-with-offset dxp (length x) pad pad))
  (def db (reduce + 0 (reduce + 0 dy)))
  (values dx db dw))







(struct layer-class (layer forward-f backward-f))

; Fully connected layer
(struct fc-layer (weights bias))

(def (fc-layer-forward))
(def (fc-layer-backward))

(def (make-fc-layer (weights all) (bias 0))
  (layer-class (fc-layer weights bias) fc-layer-forward fc-layer-backward))

; Convolutional layer
; weights are n+1 dims, n is number of dimensions in data, extra dim is for multiple filters
; no bias because YOLO does not use it (it uses batch norm layer in all conv layers instead)
(struct conv-layer (weights stride pad))

; from darknet - convolutional_layer.c
#|
    int m = l.n/l.groups;
    int k = l.size*l.size*l.c/l.groups;
    int n = l.out_w*l.out_h;
    for(i = 0; i < l.batch; ++i){
        for(j = 0; j < l.groups; ++j){
            float *a = l.weights + j*l.nweights/l.groups;
            float *b = net.workspace;
            float *c = l.output + (i*l.groups + j)*n*m;
            float *im =  net.input + (i*l.groups + j)*l.c/l.groups*l.h*l.w;

            if (l.size == 1) {
                b = im;
            } else {
                im2col_cpu(im, l.c/l.groups, l.h, l.w, l.size, l.stride, l.pad, b);
            }
            gemm(0,0,m,n,k,1,a,k,b,n,1,c,n);
        }
    }


M - number of filter in a layer
N - output size (w * h for 2d)
K - input size (w * h * channels for 2d)
A - weights
lda -

B - input
ldb -

C - output
ldc -

for each filter with index i:
  for each pixel in input with index k:
    for each pixel in output with index j:
      
void gemm_nn_simple(int M, int N, int K,
        float *A, int lda, 
        float *B, int ldb,
        float *C, int ldc)
{
    int i,j,k;
    #pragma omp parallel for
    for(i = 0; i < M; ++i){
        for(k = 0; k < K; ++k){
            for(j = 0; j < N; ++j){
                C[i*ldc+j] += A[i*lda+k]*B[k*ldb+j];
            }
        }
    }
}
|#
(def (conv-layer-forward))
(def (conv-layer-backward))

(def (make-conv-layer (filter-size 1) (filter-num 0) (stride 0) (pad 0))
  (layer-class (conv-layer weights bias stride pad) conv-layer-forward conv-layer-backward))

; Activation layer
(struct act-layer (act-f act-f-prime))

(def (act-layer-forward))
(def (act-layer-backward))

(def (make-act-layer () () ()))

; Actiovation function + derivatives
(def (leaky-relu (x 0))
  (select (> x 0) x (* 0.1 x)))

(def (leaky-relu-prime (x 0))
  (select (> x 0) 1 0.1))

(def (linear (x 0)) x)

(def (linear-prime (x 0)) 1)

; Max pooling layer
(struct max-pool (size))

(def (max-pool-layer-forward))
(def (max-pool-layer-backward))

; Dropout layer
(struct dropout (prob))

(def (dropout-layer-forward))
(def (dropout-layer-backward))

(struct detection ())

; Batch-normalization layer



; layers is [gen-layer]
(struct network (layers input output))






































