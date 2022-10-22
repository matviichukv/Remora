#lang remora/dynamic

; TODO list to improve this code
; 1. replace get-windows in conv-layer-backward with a function that correctly
;    gets the numbers
; 2. Abstract away layers in structs, with each field in the struct being either a single number or a box with arrays.
; 3. Add a config parser to generate layer from files
; 4. add a non-training main method.

(def (showln (a all))
  (show a) (printf "\n"))


; Pads the given tensor with 0's on each dimension, on each side, pad is the number of 0's to put in on each side
(def (tensor-pad (x all) (pad 0))
  (def new-shape (+ (shape-of x) (* 2 pad)))
  (def origin ((fn ((_ 0)) (- pad)) new-shape))
  (slice/fill x origin new-shape 0))
#;
(tensor-pad (iota [3 3]) 1)

; Fully connected layer
(struct fc-layer (weights bias))
; input has to be flattened
; weights is of dimension <size of output> x <size of input>
; bias is of size <size of output>
(def (fc-layer-forward (input 1) (weights 2) (bias 1))
  (+ bias (reduce-n + 0 (#r(1 1)* input weights) 1)))

; backpropagation of the fully connected layer
; output is dx (of size input), dw (of size w), db (size - first dimension of w)
(def (fc-layer-backward (input 1) (dy 1) (w 2))
  (def dx-zero ((fn ((_ 0)) 0) input))
  (def dx (reduce + dx-zero (* dy w)))
  (def dw (#r(0 1)* dy input))
  (def db (reduce + 0 dy))
  (values dx dw db))

; compute windows of shape window-shape, starting at top-left point of the array (0,0,...)
; stride is the offset from the previous window's origin
; example: input = (iota [4 4])
;          window-shape = [2 2]
(def (get-windows (input all) (window-shape 1) (stride 0))
  (def input-shape (shape-of input))
  (def (calc-size (n 0) (window-size 0)) (add1 (floor (/ (- n window-size) stride))))
  (def output-shape (calc-size input-shape window-shape))
  ; we have to use list and map here because our indices can be different and
  ; they can produce a jagged array which is slow/annoying to deal with.
  ; the output after cartesian product will be uniform tho so it should be fine
  (def output-shape-list (array->nest-list output-shape))
  (def output-indices (map (fn ((top 0)) (* stride (range 0 top 1))) output-shape-list))
  (def all-output-window-offsets (cartesian-product output-indices))
  (def all-windows (slice input all-output-window-offsets window-shape))
  (reshape (append output-shape window-shape) all-windows))

; returns total number of elements in the given array
(def (num-elts (x all))
  (reduce * 1 (shape-of x)))

; compute the mean of the array x
(def (mean (x all))
  (/ (reduce-n + 0 x (length (shape-of x)))
     (num-elts x)))

; computes the variance over x, with mean being the mean of that same array.
; you pass in mean to save on this duplicate computation
(def (variance (x all) (mean 0))
  (/ (reduce-n + 0 (expt (- x mean) 2) (length (shape-of x)))
     (num-elts x)))

; Convolutional layer
; weights are n+1 dims, n is number of dimensions in data, extra dim is for multiple filters
; returns a output size for a dimenstion given input size n and filter size w-size
(def (conv-output-size (n 0) (w-size 0) (pad 0) (stride 0))
  (add1 (floor (/ (+ n (* 2 pad) (- w-size)) stride))))

; w is the list of filters
; computes a convolition, input is data,
; w is an array of arrays of weights,
; pad is padding for input, e.g. [1 2 3] with pad 1 -> [0 1 2 3 0]; [[1 2] [3 4]] with pad 0 -> [[0 0 0 0] [0 1 2 0] [0 3 4 0] [0 0 0 0]]
; batch-normalize? is whether the output of this layer is going to be normalized in the future, if not, then bias is added
; stride is window stride - by how much to slide the input, e.g.
; when doing convolution for input (iota [4 4]) with w of size [2 2]
; stride 1: [[0 1] [4 5]] convolve with w; [[1 2] [5 6]] convolve with w
; stride 2: [[0 1] [4 5]] convolve with w; [[2 3] [6 7]] convolve with w
(def (conv-layer-forward (input all) (w all) (b 1) (pad 0) (stride 0) (batch-normalize? 0))
  (define single-w-shape (drop 1 (shape-of w)))
  (define padded-input (tensor-pad input pad))
  (def all-windows (get-windows padded-input single-w-shape stride))
  (println "constructed windows for conv")
  (def w-replicated (#r(1 -1)reshape (shape-of all-windows) w))
  (println "doing multiplication + reduce now")
  (def pre-bias-result (reduce-n + 0 (#r(-1 all)* w-replicated all-windows) (length single-w-shape)))
  ; do not add bias if we're doing batch normalization
  (select batch-normalize? pre-bias-result (+ b pre-bias-result)))

(conv-layer-forward (iota [5 5]) [(iota [3 3])] [1] 0 1 #f)

; w adn dy are all weights/filters (not a single one
; this computes a backwards pass and returns deltas for w and input
; FIXME: does not currently work for stride other than 1
(def (conv-layer-backward (dy all) (w all) (input all) (pad 0) (stride 0))
  (def mid-net-conv-layer? (> (length (shape-of w)) (length (shape-of dy))))
  (print "mid net conv layer = ") (showln mid-net-conv-layer?)
  (print "full w-shape")
  (showln (shape-of w))
  (define w-shape (drop 1 (shape-of w)))
  (define dy-shape (drop 1 (shape-of dy)))
  (define padded-input (#r(-1 0)tensor-pad input pad))
  (def all-input-windows ((select mid-net-conv-layer?
                                  (fn () (#r(-1 all all)get-windows padded-input dy-shape stride))
                                  (fn () (get-windows padded-input dy-shape stride)))))
  (showln (shape-of all-input-windows))
  (showln (shape-of dy))
  (def dy-replicated (#r(1 -1)reshape (shape-of all-input-windows) dy))
  (println "dy-replicated done")
  (def dw (reduce-n + 0 (#r(all -1)* all-input-windows dy-replicated) (sub1 (length (shape-of dy)))))
  (println "dw done")
  (print "w-shape")
  (showln w-shape)
  (def dy-pad-amt (sub1 (select mid-net-conv-layer? (index w-shape [1]) (index w-shape [0]))))
  (println "dy-pad-amt")
  (showln dy-pad-amt)
  (def pad-dy (#r(-1 all)tensor-pad dy dy-pad-amt))
  (println "constructed pad-dy")
  (print "shape of pad-dy")
  (showln (shape-of pad-dy))
  (def mid-net-dy-windows-compute (fn ()
    (def correct-window-shape (drop 1 w-shape))
    (def additional-idx (index w-shape [0]))
    (def almost-correct-windows (#r(-1 1 0)get-windows pad-dy correct-window-shape stride))
    ; add a dimention to have the correct shape
    (#r(1 -1)reshape (append [additional-idx] (drop 1 (shape-of almost-correct-windows)))
                     almost-correct-windows)))
  (def pad-dy-inputs ((select mid-net-conv-layer?
                       mid-net-dy-windows-compute
                       (fn () (#r(-1 1 0)get-windows pad-dy w-shape stride)))))
  #;(def pad-dy-inputs (#r(-1 1 0)get-windows pad-dy w-shape stride))
  (print "shape-of pad-dy-inputs")
  (showln (shape-of pad-dy-inputs))
  (def w-replicated ((select mid-net-conv-layer?
                             (fn () (#r(1 -2)reshape (drop 2 (shape-of pad-dy-inputs)) w))
                             (fn () (#r(1 -1)reshape (drop 1 (shape-of pad-dy-inputs)) w)))))
  ;(def w-replicated (#r(1 -1)reshape (drop 1 (shape-of pad-dy-inputs)) w)) ; replicating w to have
  (def unfolded-dxp (* pad-dy-inputs w-replicated))
  (def dxp (reduce + ((fn ((_ 0)) 0) (iota (shape-of input))) (reduce-n + 0 unfolded-dxp
                                                                        (select mid-net-conv-layer? (sub1 (length w-shape)) (length w-shape)))))
  
  (def db (reduce-n + 0 dy (length dy-shape)))
  
  (values dxp dw db))
#;
(conv-layer-backward [[[0.5 0.5 0.5 0.5]
                       [0.5 0.5 0.5 0.5]
                       [0.5 0.5 0.5 0.5]
                       [0.5 0.5 0.5 0.5]]
                      [[0.75 0.75 0.75 0.75]
                       [0.75 0.75 0.75 0.75]
                       [0.75 0.75 0.75 0.75]
                       [0.75 0.75 0.75 0.75]]]
                     [[[1 2]
                       [3 4]]
                      [[1 1]
                       [1 1]]]
                     [[2 1 3 4 2]
                      [2 5 1 2 2]
                      [4 3 3 1 2]
                      [2 3 3 4 2]
                      [1 3 3 2 2]]
                     0
                     1)
#;
(conv-layer-backward (iota [64 24 24]) (iota [64 32 3 3]) (iota [32 26 26]) 0 1)

; Activation layer
; forward pass for activation layer
(def (act-layer-forward (input all) (f 0))
  (f input))
; backward pass for activation layer
(def (act-layer-backward (dy all) (f-prime 0))
  (* (f-prime dy) dy))

(def (make-act-layer) 1)

; Actiovation function + derivatives
(def (leaky-relu (x 0))
  (select (> x 0) x (* 0.1 x)))

(def (leaky-relu-prime (x 0))
  (select (> x 0) 1 0.1))

(def (linear (x 0)) x)

(def (linear-prime (x 0)) 1)

(def (softmax (x 1))
  (def e (exp x))
  (/ e (reduce + 0 e)))

; truth is one-hot encoding vector - there can only be 1 1.0 value, everything else is 0.0
; x is the output of softmax function
(def (softmax-prime (x 1) (truth 1))
  (- x truth))

; turns softmax output vector to one-hot encoded vector -
; max value turns into 1, all others into 0
(def (softmax->class-vector (e 1))
  (def max-val (reduce max -inf.0 e))
  ((fn ((v 0)) (select (equal v max-val) 1.0 0.0)) e))
#;
(softmax [1 2 4 3 5])

; flatten layer (to feed into fully connected layers)
(def (flatten-layer-forward (input all))
  (reshape [(num-elts input)] input))

(def (flatten-layer-backward (input all) (dy 1))
  (reshape (shape-of input) dy))

;returns an array of shape (append (shape-of a) (length (shape-of a)))
; this array has the same shape as a, and each cell is replaced with an index of it
(def (get-index-arr (a all))
  (def input-shape (shape-of a))
  (def input-shape-list (array->nest-list input-shape))
  (def output-indices (map (fn ((top 0)) (range 0 top 1)) input-shape-list))
  (reshape (append input-shape [(length input-shape)]) (cartesian-product output-indices)))

; Max pooling layer
; input is single input data 'frame'
; Computes forward pass for max pooling layer
; Takes a maximum value of all size-sized tensors (shape is size*size*size..., equal to rank of input)
; e.g. input (iota [4 4]), stride 2, size 2, pad 0
; output: [[5 7] [13 15]]
(def (max-pool-layer-forward (input all) (stride 0) (size 0) (pad 0))
  (def padded-input (tensor-pad input pad))
  (def padded-input-shape (shape-of padded-input))
  (def pool-shape ((fn ((_ 0)) size) padded-input-shape))
  (def windows (get-windows padded-input pool-shape stride))
  (def padded-index-arr (get-index-arr padded-input))
  (def index-windows (get-windows padded-index-arr (append pool-shape [(length padded-input-shape)]) stride))
  (def invalid-idx ((fn ((_ 0)) -inf.0) (shape-of input)))
  (def (max-index (a 1) (b 1))
    (select (equal a invalid-idx)
            b
            (select (equal b invalid-idx)
                    a
                    (select (> (index padded-input a) (index padded-input b))
                            a
                            b))))
  (def index-output (reduce-n-2 max-index invalid-idx index-windows (add1 (length padded-input-shape))))
  (def output (reduce-n max -inf.0 windows (length padded-input-shape)))
  (values output index-output))
#;
(def-values (a b) (max-pool-layer-forward (iota [4 4]) 2 2 1))
; idea: in forward, construct an array size of output, where each cell is the index of the original value
;       then in backward, do a fold over output-sized index array, and do the multiply thing from the two arrays (dy and index of orig values)
; another idea: implement reduce that can accept multiple arrays so it's easier, or something like a for/fold


; computes the backward propagation for max pooling layer
; dy is delta for output, dy-input-index is the index of cell where the value came from in forward pass
; output is of size of input
(def (max-pool-layer-backward (input all) (dy all) (dy-input-index all) (pad 0))
  (def padded-input (tensor-pad input pad))
  (def input-shape (shape-of input))
  (def index-arr (reshape [(num-elts dy) (length (shape-of dy))] (get-index-arr dy)))
  (def dx-zero ((fn ((_ 0)) 0) padded-input))
  (def (fold-replace-fn (idx 1) (acc-arr all))
    (define input-idx (index dy-input-index idx))
    (define input-dy  (index dy idx))
    (array-set acc-arr input-idx (+ input-dy (index acc-arr input-idx))))
  (def padded-dx (foldr fold-replace-fn dx-zero index-arr))
  (slice padded-dx ((fn ((_ 0)) pad) input-shape) input-shape))
#;
(def a-back (max-pool-layer-backward (iota [4 4]) ((fn ((_ 0)) 0.6) (iota [3 3])) b 1))

; Dropout layer
; rand-number-gen for consistent randomness during debugging
; prob - float 0 <= x <= 1, probability of leaving the number in (vs dropping and replacing with a 0)
; scale - multiplier for selected values
(def (dropout-layer-forward (input all) (prob 0) (rand-number-gen 0))
  (def scale (/ 1 (- 1 prob)))
  (def input-shape (shape-of input))
  (def random-numbers (build-array (fn ((_ 0)) (random rand-number-gen)) input-shape))
  (def dropout-filter (< random-numbers prob))
  (def dropout-select (fn ((bool 0) (x 0)) (select bool 0 (* scale x))))
  (def dropout-result (dropout-select dropout-filter input))
  (values dropout-result dropout-filter))
#;
(def-values (d-res d-fil) (dropout-layer-forward (iota [4 4]) 0.5 1 (current-pseudo-random-generator)))

; dropout back pass
; output shape = input shape
; only propagates changes that were not dropped by the layer
; input-filter is the result from forward pass, which shows dropped vs left in inputs (matrix of booleans)
; scale is a scalar multiplier for dy coefficients
(def (dropout-layer-backward (input-filter all) (dy all) (scale 0))
  (def dropout-back (fn ((bool 0) (x 0)) (select bool 0 (* x scale))))
  (dropout-back input-filter dy))
#;
(dropout-layer-backward d-fil (build-array (fn ((_ 0)) (random)) [4 4]) 1)


; w and h are normalized - 1.0 means the width of the image
; x and y are normalized - offsets from grid cell, where 1.0 is the size of the grid cell box (which is 1/s, s is the side of the output)
; also x and y are the center of the box
(struct yolo-box (x y w h confidence))

; v is the vector to format so it can be passed to detection layer
; n is the number of boxes
; num-classes is number of classes to predict
; Precondition: (length v) = n * 5 + num-classes
; returns a vector, where first n elements are yolo-box'es, and the rest are class predictions
(def (vector-to-yolo-output (v 1) (n 0) (num-classes 0))
  ; turn first 5 elements of a vector into a yolo-box cause structs have some weird application rules
  ; length of vector should ideally be 5 elements
  (def (vector-to-yolo-box (v 1))
    (yolo-box (index v [0]) (index v [1]) (index v [2]) (index v [3]) (index v [4])))
  (def box-slice (slice v [0] [(* 5 n)]))
  
  (def boxes-unshaped (vector-to-yolo-box (reshape [n 5] box-slice)))
  (def boxes (reshape [n] boxes-unshaped))
  (append boxes (slice v [(* n 5)] [num-classes])))

(vector-to-yolo-output (iota [20]) 2 10)

; turns a yolo-box into a vector with 5 elementes, [x y w h confidence]
(def (yolo-box-to-vector (box 0))
  [(yolo-box-x box) (yolo-box-y box) (yolo-box-w box) (yolo-box-h box) (yolo-box-confidence box)])

; rmse of two yolo boxes
; rmse - root mean square error
(def (yolo-box-rmse (a 0) (b 0))
  (sqrt (+ (expt (- (yolo-box-x a) (yolo-box-x b)) 2)
           (expt (- (yolo-box-y a) (yolo-box-y b)) 2)
           (expt (- (yolo-box-w a) (yolo-box-w b)) 2)
           (expt (- (yolo-box-h a) (yolo-box-h b)) 2))))

; overlap between 
(def (overlap (x1 0) (w1 0) (x2 0) (w2 0))
  (def l1 (- x1 (/ w1 2)))
  (def l2 (- x2 (/ w2 2)))
  (def left (select (> l1 l2) l1 l2))
  (def r1 (+ x1 (/ w1 2)))
  (def r2 (+ x2 (/ w2 2)))
  (def right (select (< r1 r2) r1 r2))
  (- right left))

; are of intersection of two yolo boxes
(def (yolo-box-intersect (a 0) (b 0))
  (def w (overlap (yolo-box-x a) (yolo-box-w a) (yolo-box-x b) (yolo-box-w b)))
  (def h (overlap (yolo-box-y a) (yolo-box-h a) (yolo-box-y b) (yolo-box-h b)))
  (def area (* w h))
  (select (< area 0) 0 area))

; area of union of two yolo boxes
(def (yolo-box-union (a 0) (b 0))
  (def i (yolo-box-intersect a b))
  (+ (* (yolo-box-w a) (yolo-box-h a))
     (* (yolo-box-w b) (yolo-box-h b))
     (- i)))

; intersction area of two yolo boxes divided by union of two yolo boxes
(def (yolo-box-iou (a 0) (b 0))
  (/ (yolo-box-intersect a b)
     (yolo-box-union a b)))
  

; the delta function to use for the box that's the closest to the truth box
; pred-box (of type yolo-box) is the box that was predicted by the neural net
; truth-box (of type yolo-box) is the actual box with an object
; coord-scale is a factor of how much we scale the difference for coordinates
; obj-scale is a factor of how much we scale the difference in confidence
; sqrt-flag is a flag, we will square the size of the box and then sqrt the difference from the truth box
; rescore-flag is whether we should use iou vs 1 for calculating the confidence score (#t - use iou, #g - use 1
; returns a yolo-box that contains the deltas for each of the fields
(def (best-yolo-box-delta (pred-box 0) (truth-box 0) (coord-scale 0) (obj-scale 0) (sqrt-flag 0) (rescore-flag 0))
  (def new-w (select sqrt-flag
                     (sqrt (- (yolo-box-w truth-box) (expt (yolo-box-w pred-box) 2)))
                     (- (yolo-box-w truth-box) (yolo-box-w pred-box))))
  (def new-h (select sqrt-flag
                     (sqrt (- (yolo-box-h truth-box) (expt (yolo-box-h pred-box) 2)))
                     (- (yolo-box-h truth-box) (yolo-box-h pred-box))))
  (def iou (yolo-box-iou truth-box pred-box))
  (def delta-p (select rescore-flag
                    (* obj-scale (- iou (yolo-box-confidence pred-box)))
                    (* obj-scale (- 1 (yolo-box-confidence pred-box)))))
  (yolo-box (- (yolo-box-x truth-box) (yolo-box-x pred-box))
            (- (yolo-box-y truth-box) (yolo-box-y pred-box))
            new-w
            new-h
            delta-p))

(best-yolo-box-delta (yolo-box 0.1 0.2 0.3 0.3 0.4) (yolo-box 0.5 0.6 0.7 0.8 0.9) 1 1 #t #t)

; the delta function to use for the boxes that are not the closest to truth box
; pred-box (of type yolo-box) is the box that neural net predicted
; noobj-scale is how much we scale the negative delta for confidence
; returns a yolo-box that contains the deltas for each of the fields
(def (other-yolo-box-delta (pred-box 0) (noobj-scale 0))
  (def delta-p (* noobj-scale (- (yolo-box-confidence pred-box))))
  ; we don't do negative delta for coords on worse guesses for some reason
  (yolo-box 0 0 0 0 delta-p))

; merge a vector of yolo-boxes and the vector of class deltas into a single vector of numbers
; returns a vector of size: (+ (* 5 (len boxes-delta)) (len class-delta))
(def (merge-box-delta-and-class-delta (boxes-delta 1) (class-delta 1))
  (append (reshape [(* 5 (length boxes-delta))] (yolo-box-to-vector boxes-delta))
          class-delta))

; input should be of dimension: <shape of cells>x<shape of detection res>
; shape of cells - (w x h for imgs. w x h x time for video)
; shape of detection res is <number of boxes per cell> + <number of classes>
; each box is the struct yolo-box
; returns the input as it's the prediction of the yolo network + dy of the same shape as input that's the delta
; n > 0
; computes the loss function for the given input
; all *-scale arguments are coefficients defined by the YOLO network specification
(def (detection-forward (input 1) (truth 1) (classes 0) (side 0) (n 0) (sqrt 0)
                        (obj-scale 0) (noobj-scale 0) (coord-scale 0) (class-scale 0))
  (def input-gen-shape (drop-right 1 (shape-of input)))
  (def locations (* side side))
  (def boxes-pred (slice input [0] [n]))
  (def class-pred (slice input [n] [classes]))
  (def truth-box (index truth [0]))
  (showln truth-box)
  (def class-truth (slice truth [1] [classes]))


  ; class delta stuff
  (def class-delta (* class-scale (- class-truth class-pred)))
  (def class-cost (reduce + 0 (* class-scale (expt (- class-truth class-pred) 2))))
  ; box delta stuff
  
  (struct yolo-box-stats (idx iou rmse))
  ; a is a best-box-stats and b is a single yolo-box
  ; double chech that this is correct, maybe need to 
  (def (box-cmp (truth 0))
    (fn ((acc 0) (next 0))
        (printf "acc: ~v; next: ~v\n" acc next)
        (def best (car acc))
        (def cur-idx (cdr acc))
        (def iou (yolo-box-iou next truth))
        (def rmse (yolo-box-rmse next truth))
        (def best-iou (yolo-box-stats-iou best))
        (def best-rmse (yolo-box-stats-rmse best))
        (select (or (> best-iou 0) (> iou 0))
                (select (> iou best-iou) (cons (yolo-box-stats cur-idx iou rmse) (add1 cur-idx)) (cons best (add1 cur-idx)))
                (select (< rmse best-rmse) (cons (yolo-box-stats cur-idx iou rmse) (add1 cur-idx)) (cons best (add1 cur-idx))))))
  (def first-box (head boxes-pred))
  (print "first-box ") (showln first-box)
  (def first-box-stats (yolo-box-stats 0 (yolo-box-iou first-box truth-box) (yolo-box-rmse first-box truth-box)))
  (def rest-boxes (behead boxes-pred))
  (print "rest-box ") (showln rest-boxes)
  ; could be done with a reduce but much simplifies the code to calculate all the deltas
  (showln (cons first-box-stats 1))
  (def best-box-and-idx (foldl (box-cmp truth-box) (cons first-box-stats 1) rest-boxes))
  (showln best-box-and-idx)
  (def best-box-info (car best-box-and-idx))
  (def best-box-idx (yolo-box-stats-idx best-box-info))
  (showln best-box-info)

  (def (calc-box-delta (box 0) (truth-box 0) (idx 0))
    (select (equal? idx best-box-idx)
            (best-yolo-box-delta box truth-box coord-scale obj-scale sqrt #t) ; #t is rescore flag
            (other-yolo-box-delta box noobj-scale)))

  (showln (append input-gen-shape [n]))
  (showln (shape-of boxes-pred))
  (def delta-boxes (calc-box-delta boxes-pred truth-box (iota [n])))
  (showln (shape-of delta-boxes))
  (showln (shape-of class-delta))
  (def delta-output (merge-box-delta-and-class-delta delta-boxes class-delta))
  (values input delta-output))
#;
(detection-forward [[(yolo-box 0.5 0.5 0.5 0.5 0.7) (yolo-box 0.5 0.5 0.5 0.5 0.7) 0.1 0.5 0.3 0.4 0.2]
                    [(yolo-box 0.5 0.5 0.5 0.5 0.7) (yolo-box 0.5 0.5 0.5 0.5 0.7) 0.1 0.5 0.3 0.4 0.2]]
                   [[(yolo-box 0.1 0.2 0.3 0.4 0.8) 1 0 0 0 0]
                    [(yolo-box 0.1 0.2 0.3 0.4 0.8) 1 0 0 0 0]]
                   5 1 2 1 1 0.5 5 1)

; we computed the dy in detection-forward layer, so no need for backprop in detection layer

; rolling-mean size = rolling-var size = bias size = number of channels in input (first dimesion)
; train is a bool whether it is training
; bias is a vector of scalars that get applied to each layer after computing statistics things
(def (batch-norm-forward (input all) (rolling-mean 1) (rolling-var 1) (bias 1) (train 0))
  (def input-mean (#r(-1)mean input))
  (def input-var  (#r(-1 0)variance input input-mean))
  (def new-rolling-mean (+ (* 0.99 rolling-mean) (* 0.01 input-mean)))
  (def new-rolling-var (+ (* 0.99 rolling-var) (* 0.01 input-var)))
  (def mean-to-use (select train input-mean rolling-mean))
  (def var-to-use  (select train input-var rolling-var))
  (showln (- input mean-to-use)) 
  (showln (+ (sqrt var-to-use) 0.000001)) 
  (def normalized-output (/ (- input mean-to-use) (sqrt (+ var-to-use 0.000001))))
  (values (+ normalized-output bias) input-mean input-var new-rolling-mean new-rolling-var))
#;
(def-values (batch-norm-out foo bar roll-mean roll-var)
  (batch-norm-forward [[[2 6 1]
                        [1 5 0]
                        [2 9 7]]] [0] [0] [0] #t))

; dy is delta of the output, shape of dy = shape of input
; mean and var are the ones computed in forward pass
; rolling-mean and rolling-var are the ones in the layer
; train is where we are in the training or predicting mode
; returns dx, db (delta in bias), dmean - delta in mean, dvar - delta in var
(def (batch-norm-backward (dy all) (input all) (mean 1) (var 1) (rolling-mean 1) (rolling-var 1) (train 0))
  ; spatial is the number of elements in each channel of dy
  (def spatial (reduce * 1 (drop 1 (shape-of dy))))
  ; we need to use different mean/var in whether we are training or predicting
  ; 0.00001 constant is taken from darknet code, some numerical magic
  (def var-to-use  (+ 0.00001 (select train mean rolling-mean)))
  (def mean-to-use (+ 0.00001 (select train var rolling-var)))
  (def db (reduce-n + 0 dy (sub1 (length (shape-of dy)))))
  (def dmean (* -1 (/ (reduce-n + 0 dy (sub1 (length (shape-of dy)))) (sqrt var-to-use))))
  (def dvar-wip (reduce-n + 0 (* dy (- input mean-to-use)) (sub1 (length (shape-of dy)))))
  (def dvar (* dvar-wip -0.5 (expt var-to-use -3/2)))
  (def dx (+ (/ dy (sqrt var-to-use))
             (/ (* 2 dvar (- input mean-to-use)) spatial)
             (/ dmean spatial)))
  (values dx db))
#|
(def-values (f1 f2 f3 f4 f5) (batch-norm-forward [[1 2] [3 4] [100 100000]] [0 0 0] [0 0 0] [0 0 0] 1))
(def-values (batch-dx batch-db) (batch-norm-backward [[0 0] [-1 0] [1 0]] [[1 2] [3 4] [100 100000]] f2 f3 f4 f5 1))
(println "batch norm in order: dx db")
(showln batch-dx)
(showln batch-db)
|#


;;; ------------ RUNTIME TRAINING CODE --------------------------------------------------------------------------------------

; generates a random array of the given size
(def (generate-random-array (shape 1) (rand-number-gen 0))
  ((fn ((_ 0)) (random rand-number-gen)) (iota shape)))

(def start (current-seconds))
; nn-data is the vector of boxes which contain all the weights/etc needed to run the nn
; returns the same vector of boxes with updated weights
(def (yolo-train (nn-data 1) (input 2) (truth 1) (num-boxes 0) (num-classes 0) (side 0) (learning-rate 0) (random-num-gen 0))
  ; get all the weights needed
  (def w1 (unsafe-unbox (index nn-data [0])))
  (def b1 (unsafe-unbox (index nn-data [1])))
  (def roll-var1 (unsafe-unbox (index nn-data [2])))
  (def roll-mean1 (unsafe-unbox (index nn-data [3])))
  (def w2 (unsafe-unbox (index nn-data [4])))
  (def b2 (unsafe-unbox (index nn-data [5])))
  (def roll-var2 (unsafe-unbox (index nn-data [6])))
  (def roll-mean2 (unsafe-unbox (index nn-data [7])))
  (def w6 (unsafe-unbox (index nn-data [8])))
  (def b6 (unsafe-unbox (index nn-data [9])))
  (def w8 (unsafe-unbox (index nn-data [10])))
  (def b8 (unsafe-unbox (index nn-data [11])))
  (def roll-var8 (unsafe-unbox (index nn-data [12])))
  (def roll-mean8 (unsafe-unbox (index nn-data [13])))
  (println "starting the nn")
  ; na suffix is not activated
  (def out1-na (conv-layer-forward input w1 b1 0 1 #t))
  (def-values (out1-norm out1-mean out1-var out1-roll-mean out1-roll-var) (batch-norm-forward out1-na roll-mean1 roll-var1 b1 #t))
  (def out1 (act-layer-forward out1-na leaky-relu))
  (println "layer 1 done")
  (print "shape of out: ")
  (showln (shape-of out1))
  (printf "time took so far: ~v\n" (- (current-seconds) start))
  (def out2-na (conv-layer-forward out1 w2 b2 0 1 #t))
  (println "layer 2 conv done")
  (print "shape of out: ")
  (showln (shape-of out2-na))
  (printf "time took so far: ~v\n" (- (current-seconds) start))
  (def out2-shaped (reshape (filter (not (equal? 1 (shape-of out2-na))) (shape-of out2-na)) out2-na))
  (def-values (out2-norm out2-mean out2-var out2-roll-mean out2-roll-var) (batch-norm-forward out2-shaped roll-mean2 roll-var2 b2 #t))
  (def out2 (act-layer-forward out2-norm leaky-relu))
  (println "layer 2 done")
  (print "shape of out: ")
  (showln (shape-of out2))
  (printf "time took so far: ~v\n" (- (current-seconds) start))
  (def-values (out3 out3-index) (#r(-1 0 0 0)max-pool-layer-forward out2 1 2 0))
  (println "layer 3 done")
  (print "shape of out: ")
  (showln (shape-of out3))
  (printf "time took so far: ~v\n" (- (current-seconds) start))
  (def-values (out4 out4-filter) (dropout-layer-forward out3 0.25 random-num-gen))
  (println "layer 4 done")
  (print "shape of out: ")
  (showln (shape-of out4))
  (printf "time took so far: ~v\n" (- (current-seconds) start))
  (def out5 (flatten-layer-forward out4))
  (println "layer 5 done")
  (print "shape of out: ")
  (showln (shape-of out5))
  (printf "time took so far: ~v\n" (- (current-seconds) start))
  (def out6-na (fc-layer-forward out5 w6 b6))
  (def out6 (act-layer-forward out6-na leaky-relu))
  (println "layer 6 done")
  (print "shape of out: ")
  (showln (shape-of out6))
  (printf "time took so far: ~v\n" (- (current-seconds) start))
  (def-values (out7 out7-filter) (dropout-layer-forward out6 0.5 random-num-gen))
  (println "layer 7 done")
  (print "shape of out: ")
  (showln (shape-of out7))
  (printf "time took so far: ~v\n" (- (current-seconds) start))
  (def out8-na (fc-layer-forward out7 w8 b8))
  (print "out8-na")(showln out8-na)
  ; the norm layer here is a bit of a hack because we have a lot of numbers, so summing up even really
  ; small numbers a lot of time gives really big numbers (like 30k on one of the steps)
  ; should not be a problem once we have a full-scale YOLO since more conv layers will give the
  ; smaller number of values that's needed to avoid this problem
  (def-values (out8-norm out8-mean out8-var out8-roll-mean out8-roll-var)
    (batch-norm-forward (reshape [1 10] out8-na) roll-mean8 roll-var8 [0] #t))
  (print "out: ")
  (showln out8-norm)
  (print "in: ")
  (showln out7)
  (def out8 (act-layer-forward (reshape [10] out8-norm) softmax))
  (println "layer 8 done")
  (print "shape of out: ")
  (showln (shape-of out8))
  (println "layer 8 out: ")
  (showln out8)
  (printf "time took so far: ~v\n" (- (current-seconds) start))

  ; to deal with case where you have multiple cells, just reshape the output to [side side <yolo-output-size>]
  ; and apply the function to that result
  ; yolo-output-size = num-boxes * 5 + num-classes
  (def yolo-output (vector-to-yolo-output out8 num-boxes num-classes))
  (def yolo-truth (vector-to-yolo-output truth num-boxes num-classes))

  ; constants here are copied from darknet
  (def-values (_idk delta) (detection-forward yolo-output yolo-truth num-classes side num-boxes #t 1 0.5 5 1))

  (print "the detection function delta: ")(showln delta)
  
  ; start-backprop
  (def dout8-na (softmax-prime delta truth))
  (def-values (dout8-norm _db8) (batch-norm-backward (reshape [1 10] dout8-na) (reshape [1 10] out8-na) out8-mean out8-var out8-roll-mean out8-roll-var #t))
  (def-values (dout8 dw8 db8) (fc-layer-backward out7 (reshape [10] dout8-norm) w8))
  (println "layer 8 back done")
  (printf "time took so far: ~v\n" (- (current-seconds) start))
  (def dout7 (dropout-layer-backward out7-filter dout8 0.5))
  (println "layer 7 back done")
  (printf "time took so far: ~v\n" (- (current-seconds) start))
  (def dout6-na (act-layer-backward dout7 leaky-relu-prime))
  (def-values (dout6 dw6 db6) (fc-layer-backward out5 dout6-na w6))
  (println "layer 6 back done")
  (printf "time took so far: ~v\n" (- (current-seconds) start))
  (def dout5 (flatten-layer-backward out4 dout6))
  (println "layer 5 back done")
  (printf "time took so far: ~v\n" (- (current-seconds) start))
  (def dout4 (dropout-layer-backward out4-filter dout5 0.25))
  (println "layer 4 back done")
  (printf "time took so far: ~v\n" (- (current-seconds) start))
  (def dout3 (#r(-1 -1 -1 0)max-pool-layer-backward out2 dout4 out3-index 0))
  (println "layer 3 back done")
  (printf "time took so far: ~v\n" (- (current-seconds) start))
  (def dout2-na (act-layer-backward dout3 leaky-relu-prime))
  (def-values (dout2-norm db2) (batch-norm-backward dout2-na out2-shaped out2-mean out2-var out2-roll-mean out2-roll-var #t))
  (def-values (dout2 dw2 _db2) (conv-layer-backward dout2-norm w2 out1 0 1))
  (println "layer 2 back done")
  (printf "time took so far: ~v\n" (- (current-seconds) start))
  (def dout1-na (act-layer-backward dout2 leaky-relu-prime))
  (def-values (dout1-norm db1) (batch-norm-backward dout1-na out1-na out1-mean out1-var out1-roll-mean out1-roll-var #t))
  (def-values (dout1 dw1 _db1) (conv-layer-backward dout1-norm w1 input 0 1))
  (println "layer 1 back done")
  (printf "time took so far: ~v\n" (- (current-seconds) start))
  (def new-w1 (- w1 (* learning-rate dw1)))
  (def new-b1 (- b1 (* learning-rate db1)))
  (def new-w2 (- w2 (* learning-rate dw2)))
  (def new-b2 (- b2 (* learning-rate db2)))
  (def new-w5 (- w6 (* learning-rate dw6)))
  (def new-b5 (- b6 (* learning-rate db6)))
  (def new-w8 (- w8 (* learning-rate dw8)))
  (def new-b8 (- b8 (* learning-rate db8)))
  [(box new-w1) (box new-b1) (box out1-roll-var) (box out1-roll-mean)
   (box new-w2) (box new-b2) (box out2-roll-var) (box out2-roll-mean)
   (box new-w5) (box new-b5) (box new-w8) (box new-b8)])


(def random-gen (current-pseudo-random-generator))
#;
(def init-nn [(box (generate-random-array [32 3 3] random-gen))
              (box (generate-random-array [32] random-gen))
              (box ((fn ((_ 0)) 0) (iota [32])))
              (box ((fn ((_ 0)) 0) (iota [32])))
              (box (generate-random-array [64 32 3 3] random-gen))
              (box (generate-random-array [64] random-gen))
              (box ((fn ((_ 0)) 0) (iota [64])))
              (box ((fn ((_ 0)) 0) (iota [64])))
              (box (generate-random-array [128 33856] random-gen))
              (box (generate-random-array [128] random-gen))
              (box (generate-random-array [10 128] random-gen))
              (box (generate-random-array [10] random-gen))
              (box ((fn ((_ 0)) 0) (iota [1])))
              (box ((fn ((_ 0)) 0) (iota [1])))])
#;
(def new-stuff (yolo-train init-nn
                           (generate-random-array [28 28] random-gen)
                           [9.2 0.3 0.1 0.1 0.6 0 0 0 1 0]
                           1
                           5
                           1
                           0.1
                           random-gen))
(def end (current-seconds))





