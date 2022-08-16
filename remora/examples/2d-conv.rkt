#lang remora/dynamic

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
  (show w-shape)
  (printf "\n")
  (show (shape-of all-windows))
  (printf "\n")
  (reduce-n + 0 (* w all-windows) (length w-shape)))

(conv-layer-forward (build-array (fn ((x 0)) 1) [2 2]) 0 1 (iota [4 4]))

; w adn dy are a single filter/delta combo, and applications are nice because w's adn dy's agree in the top dimension
(def (conv-layer-backward (dy all) (w all) (input all) (pad 0) (stride 0))
  (define w-shape (shape-of w))
  (define dy-shape (shape-of dy))
  (define padded-input (tensor-pad input pad))
  (def all-input-windows (get-windows padded-input dy-shape stride))
  (println "testing weird mult")
  (* all-input-windows dy)
  (def dw (reduce-n + 0 (* all-input-windows dy) (length (shape-of dy))))
  (def pad-dy (tensor-pad dy (sub1 (index w-shape 0))))
  (def pad-dy-size (length pad-dy))
  (def pad-dy-inputs (get-windows pad-dy w-shape stride))
  (def unfolded-dxp (* pad-dy-inputs w))
  (def dxp (reduce-n + 0 unfolded-dxp (length w-shape)))
  (values dw dxp))
#;
(debug-mode #t)
#;
(conv-layer-backward [[0.5 0.5 0.5 0.5]
                      [0.5 0.5 0.5 0.5]
                      [0.5 0.5 0.5 0.5]
                      [0.5 0.5 0.5 0.5]]
                     [[1 2]
                      [3 4]]
                     [[2 1 3 4 2]
                      [2 5 1 2 2]
                      [4 3 3 1 2]
                      [2 3 3 4 2]
                      [1 3 3 2 2]]
                     0
                     1)
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
#;
(def (max-pool-layer-backward ()) 1)

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
#;
(dropout-layer-backward d-fil (build-array (fn ((_ 0)) (random)) [4 4]) 1)

; detection layer

(struct detection (foo))

(def (yolo-box-rmse (a 0) (b 0))
  (sqrt (+ (expt (- (yolo-box-x a) (yolo-box-x b)) 2)
           (expt (- (yolo-box-y a) (yolo-box-y b)) 2)
           (expt (- (yolo-box-w a) (yolo-box-w b)) 2)
           (expt (- (yolo-box-h a) (yolo-box-h b)) 2))))

(def (overlap (x1 0) (w1 0) (x2 0) (w2 0))
  (def l1 (- x1 (/ w1 2)))
  (def l2 (- x2 (/ w2 2)))
  (def left (select (> l1 l2) l1 l2))
  (def r1 (+ x1 (/ w1 2)))
  (def r2 (+ x2 (/ w2 2)))
  (def right (select (< r1 r2) r1 r2))
  (- right left))

(def (yolo-box-intersect (a 0) (b 0))
  (def w (overlap (yolo-box-x a) (yolo-box-w a) (yolo-box-x b) (yolo-box-w b)))
  (def h (overlap (yolo-box-y a) (yolo-box-h a) (yolo-box-y b) (yolo-box-h b)))
  (def area (* w h))
  (select (< area 0) 0 area))

(def (yolo-box-union (a 0) (b 0))
  (def i (yolo-box-intersect a b))
  (+ (* (yolo-box-w a) (yolo-box-h a))
     (* (yolo-box-w b) (yolo-box-h b))
     (- i)))

(def (yolo-box-iou (a 0) (b 0))
  (/ (yolo-box-intersect a b)
     (yolo-box-union a b)))

; input should be of dimenstion: <shape of cells>x<shape of detection res>
; shape of cells is 2d (w*h for imgs. w*h*time for video)
; shape of detection res is <number of boxes per cell> + <number of classes>
; each box is the struct yolo-box
; returns the input as it's the prediction of the yolo network + dy of the same shape as input that's the delta
; n > 0
(def (detection-forward (input all) (truth all) (classes 0) (side 0) (n 0) (sqrt 0)
                        (obj-scale 0) (noobj-scale 0) (coord-scale 0) (class-scale 0))
  (def locations (* side side))
  (def boxes-pred (#r(1 1 1)slice input [0] [n]))
  (def class-pred (#r(1 1 1)slice input [n] [classes]))
  (def box-truth (#r(1 1)index truth [0]))
  (def class-truth (#r(1 1 1)slice truth [1] [classes]))
  ; class delta stuff
  (def class-delta (* class-scale (- class-truth class-pred)))
  (def class-cost (reduce + 0 (* class-scale (expt (- class-truth class-pred) 2))))
  ; confidence delta stuff
  (def confidence-delta (* noobj-scale (- (yolo-box-confidence boxes-pred))))
  ; box delta stuff
  
  (struct best-box-stats (box iou rmse))
  ; a is a best-box-stats and b is a single yolo-box
  ; double chech that this is correct, maybe need to 
  (def (box-cmp (truth 0))
    (fn ((best 0) (next 0))
        (def iou (yolo-box-iou next truth))
        (def rmse (yolo-box-rmse next truth))
        (def best-iou (best-box-stats-iou best))
        (def best-rmse (best-box-stats-rmse best))
        (select (or (> best-iou 0) (> iou 0))
                (select (> iou best-iou) (best-box-stats next iou rmse) best)
                (select (< rmse best-rmse) (best-box-stats next iou rmse) best))))
  (def first-box (#r(1)head boxes-pred))
  (def rest-boxes (#r(1)behead boxes-pred))
  (def best-box (#r(0 1 1)reduce box-cmp first-box rest-boxes))
  (def box-delta (yolo-box-delta best-box))
  (def cost (+ class-cost))
  (values input box-delta cost))

; there is no error, since we 
(def (detection-backward (dy all)) dy)

; Batch-normalization layer

(struct batch-norm (rolling-mean rolling-var))

; rolling-mean size = rolling-var size = bias size = number of channels in input (first dimesion
; train is a bool whether

(def (batch-norm-forward (input all) (rolling-mean 1) (rolling-var 1) (bias 1) (train 0))
  (def input-mean (#r(-1)mean input))
  (def input-var  (#r(-1 0)variance input input-mean))
  (def new-rolling-mean (+ (* 0.99 rolling-mean) (* 0.01 input-mean)))
  (def new-rolling-var (+ (* 0.99 rolling-var) (* 0.01 input-var)))
  (def mean-to-use (select train new-rolling-mean rolling-mean))
  (def var-to-use  (select train new-rolling-var rolling-var))
  (def normalized-output (/ (- input mean-to-use) (+ (sqrt var-to-use) 0.000001)))
  (values (+ normalized-output bias) new-rolling-mean new-rolling-var))

(def (batch-norm-backward (dy all) (input all) (mean 1) (var 1) (rolling-mean 1) (rolling-var 1) (train 0))
  (def spatial (reduce * 1 (drop 1 (shape-of dy))))
  (def var-to-use  (+ 0.00001 (select train mean rolling-mean))) ; add 0.00001 to both
  (def mean-to-use (+ 0.00001 (select train var rolling-var)))
  (def db (reduce-n + 0 dy (sub1 (length (shape-of dy)))))
  (def dmean (* -1 (/ (reduce-n + 0 dy (sub1 (length (shape-of dy)))) (sqrt var-to-use))))
  (def dvar-wip (reduce-n + 0 (* dy (- input mean-to-use)) (sub1 (length (shape-of dy)))))
  (def dvar (* dvar-wip -0.5 (expt var-to-use -3/2)))
  (def dx (+ (/ dy (sqrt var-to-use))
             (/ (* 2 dvar (- input mean-to-use)) spatial)
             (/ dmean spatial)))
  (values dx db dmean dvar))

; w and h are normalized - 1.0 means the width of the image
; x and y are normalized - offsets from grid cell, where 1.0 is the size of the grid cell box (which is 1/s, s is the side of the output)
; also x and y are the center of the box
(struct yolo-box (x y w h confidence))

(def (yolo-box-delta (box1 0) (box2 0) (sqrt-flag 0) (coord-scale 0))
  (def new-w (select sqrt-flag
                     (sqrt (- (yolo-box-w box1) (yolo-box-w box2)))
                     (- (yolo-box-w box1) (yolo-box-w box2))))
  (def new-h (select sqrt-flag
                     (sqrt (- (yolo-box-h box1) (yolo-box-h box2)))
                     (- (yolo-box-h box1) (yolo-box-h box2))))
  (yolo-box (- (yolo-box-x box1) (yolo-box-x box2))
            (- (yolo-box-y box1) (yolo-box-y box2))
            new-w
            new-h
            (- (yolo-box-confidence box1) (yolo-box-confidence box2))))
 
  
; layers is [gen-layer]
(struct network (layers input output))




#|
int locations = l.side*l.side;
float avg_iou = 0;
        int count = 0;
        *(l.cost) = 0;
        int size = l.inputs * l.batch;
        memset(l.delta, 0, size * sizeof(float));
            for (i = 0; i < locations; ++i) {
                int truth_index = i*(1+l.coords+l.classes);
                int is_obj = net.truth[truth_index];
                for (j = 0; j < l.n; ++j) {
                    int p_index = locations*l.classes + i*l.n + j;
                    l.delta[p_index] = l.noobject_scale*(0 - l.output[p_index]);
                    *(l.cost) += l.noobject_scale*pow(l.output[p_index], 2);
                }

                int best_index = -1;
                float best_iou = 0;
                float best_rmse = 20;

                if (!is_obj){
                    continue;
                }

                int class_index = i*l.classes;
                for(j = 0; j < l.classes; ++j) {
                    l.delta[class_index+j] = l.class_scale * (net.truth[truth_index+1+j] - l.output[class_index+j]);
                    *(l.cost) += l.class_scale * pow(net.truth[truth_index+1+j] - l.output[class_index+j], 2);
                    if(net.truth[truth_index + 1 + j]) avg_cat += l.output[class_index+j];
                    avg_allcat += l.output[class_index+j];
                }

                box truth = float_to_box(net.truth + truth_index + 1 + l.classes, 1);
                truth.x /= l.side;
                truth.y /= l.side;

                for(j = 0; j < l.n; ++j){
                    int box_index = index + locations*(l.classes + l.n) + (i*l.n + j) * l.coords;
                    box out = float_to_box(l.output + box_index, 1);
                    out.x /= l.side;
                    out.y /= l.side;

                    if (l.sqrt){
                        out.w = out.w*out.w;
                        out.h = out.h*out.h;
                    }

                    float iou  = box_iou(out, truth);
                    //iou = 0;
                    float rmse = box_rmse(out, truth);
                    if(best_iou > 0 || iou > 0){
                        if(iou > best_iou){
                            best_iou = iou;
                            best_index = j;
                        }
                    }else{
                        if(rmse < best_rmse){
                            best_rmse = rmse;
                            best_index = j;
                        }
                    }
                }

                if(l.forced){
                    if(truth.w*truth.h < .1){
                        best_index = 1;
                    }else{
                        best_index = 0;
                    }
                }
                if(l.random && *(net.seen) < 64000){
                    best_index = rand()%l.n;
                }

                int box_index = locations*(l.classes + l.n) + (i*l.n + best_index) * l.coords;
                int tbox_index = truth_index + 1 + l.classes;

                box out = float_to_box(l.output + box_index, 1);
                out.x /= l.side;
                out.y /= l.side;
                if (l.sqrt) {
                    out.w = out.w*out.w;
                    out.h = out.h*out.h;
                }
                float iou  = box_iou(out, truth);

                //printf("%d,", best_index);
                int p_index = locations*l.classes + i*l.n + best_index;
                *(l.cost) -= l.noobject_scale * pow(l.output[p_index], 2);
                *(l.cost) += l.object_scale * pow(1-l.output[p_index], 2);
                l.delta[p_index] = l.object_scale * (1.-l.output[p_index]);

                if(l.rescore){
                    l.delta[p_index] = l.object_scale * (iou - l.output[p_index]);
                }

                l.delta[box_index+0] = l.coord_scale*(net.truth[tbox_index + 0] - l.output[box_index + 0]);
                l.delta[box_index+1] = l.coord_scale*(net.truth[tbox_index + 1] - l.output[box_index + 1]);
                l.delta[box_index+2] = l.coord_scale*(net.truth[tbox_index + 2] - l.output[box_index + 2]);
                l.delta[box_index+3] = l.coord_scale*(net.truth[tbox_index + 3] - l.output[box_index + 3]);
                if(l.sqrt){
                    l.delta[box_index+2] = l.coord_scale*(sqrt(net.truth[tbox_index + 2]) - l.output[box_index + 2]);
                    l.delta[box_index+3] = l.coord_scale*(sqrt(net.truth[tbox_index + 3]) - l.output[box_index + 3]);
                }

                *(l.cost) += pow(1-iou, 2);
            }
        }
|#

































