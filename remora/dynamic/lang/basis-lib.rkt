#lang racket/base

(require "AD.rkt"
	 "semantics.rkt"
         "syntax.rkt"
         racket/math
         racket/vector
         racket/list
         racket/contract
         racket/sequence
         racket/provide
         (only-in racket/base [values racket-values])
         (for-syntax racket/base))
(module+ test
  (require rackunit))

;;; Remora primops are prefixed with "R_"
(provide (filtered-out
          (λ (name) (if (regexp-match #rx"^R_" name) name #f))
          (all-defined-out)))

(define (scalar x) (rem-array #() (vector x)))

(define R_id (rem-array #() (vector (rem-scalar-proc (λ (x) x) 1))))

;;;(define R_+ (rem-array #() (vector (rem-scalar-proc (λ (x y) (d+ x y)) 2))))

;;;(define R_sqrt (rem-array #() (vector (rem-scalar-proc (λ (x) (dsqrt x)) 1))))

(define R_and (rem-array #() (vector (rem-scalar-proc (λ (x y) (and x y)) 2))))
(define R_or (rem-array #() (vector (rem-scalar-proc (λ (x y) (or x y)) 2))))

(define R_neg (rem-array #() (vector (rem-scalar-proc (λ (x) (- x)) 1))))
(define R_inv (rem-array #() (vector (rem-scalar-proc (λ (x) (/ 1 x)) 1))))

(define R_signum
  (rem-array #() (vector (rem-scalar-proc (λ (x) (/ x (magnitude x))) 1))))

(define (logb b x) (/ (log x) (log b)))
(define R_logb (rem-array #() (vector (rem-scalar-proc logb 2))))
(define R_ln (rem-array #() (vector (rem-scalar-proc log 1))))
;;;(define R_log (rem-array #() (vector (rem-scalar-proc (λ (x) (dlog 10 x)) 1))))
(define R_lg (rem-array #() (vector (rem-scalar-proc (λ (x) (logb 2 x)) 1))))

(define R_bool->int
  (rem-array #() (vector (rem-scalar-proc (λ (b) (if b 1 0)) 1))))


(define-primop (R_box [arr all])
  (rem-array #() (vector (rem-box arr))))

;;; "cell-shape" here refers to the -1-cells which will be pushed around
;;; "length" is how many -1-cells there are
(define-primop (R_head [arr all])
  ;; operates on the -1-cells
  (define cell-shape (vector-drop (rem-array-shape arr) 1))
  (rem-array (vector-drop (rem-array-shape arr) 1)
             (vector-take (rem-array-data arr)
                          (for/product ([d cell-shape]) d))))
(module+ test
  (check-equal?
   (remora
    ((rerank (1) R_head)
     (alit (4 3) 0 1 2 3 4 5 6 7 8 9 10 11)))
   (remora (alit (4) 0 3 6 9)))
  (check-equal?
   (remora
    (R_head
     (alit (4 3) 0 1 2 3 4 5 6 7 8 9 10 11)))
   (remora (alit (3) 0 1 2))))

(define-primop (R_tail [arr all])
  ;; operates on the -1-cells
  (define cell-shape (vector-drop (rem-array-shape arr) 1))
  (rem-array (vector-drop (rem-array-shape arr) 1)
             (vector-take-right (rem-array-data arr)
                                (for/product ([d cell-shape]) d))))
(module+ test
  (check-equal?
   (remora
    ((rerank (1) R_tail)
     (alit (4 3) 0 1 2 3 4 5 6 7 8 9 10 11)))
   (remora (alit (4) 2 5 8 11)))
  (check-equal?
   (remora
    (R_tail
     (alit (4 3) 0 1 2 3 4 5 6 7 8 9 10 11)))
   (remora (alit (3) 9 10 11))))

(define-primop (R_behead [arr all])
  (define cell-shape (vector-drop (rem-array-shape arr) 1))
  (rem-array (vector-append
              (vector (sub1 (vector-ref (rem-array-shape arr) 0)))
              cell-shape)
             (vector-drop (rem-array-data arr)
                          (for/product ([d cell-shape]) d))))
(module+ test
  (check-equal?
   (remora
    ((rerank (1) R_behead)
     (alit (4 3) 0 1 2 3 4 5 6 7 8 9 10 11)))
   (remora (alit (4 2) 1 2 4 5 7 8 10 11)))
  (check-equal?
   (remora
    (R_behead
     (alit (4 3) 0 1 2 3 4 5 6 7 8 9 10 11)))
   (remora (alit (3 3) 3 4 5 6 7 8 9 10 11))))

(define-primop (R_curtail [arr all])
  (define cell-shape (vector-drop (rem-array-shape arr) 1))
  (rem-array (vector-append
              (vector (sub1 (vector-ref (rem-array-shape arr) 0)))
              cell-shape)
             (vector-drop-right (rem-array-data arr)
                                (for/product ([d cell-shape]) d))))
(module+ test
  (check-equal?
   (remora
    ((rerank (1) R_curtail)
     (alit (4 3) 0 1 2 3 4 5 6 7 8 9 10 11)))
   (remora (alit (4 2) 0 1 3 4 6 7 9 10)))
  (check-equal?
   (remora
    (R_curtail
     (alit (4 3) 0 1 2 3 4 5 6 7 8 9 10 11)))
   (remora (alit (3 3) 0 1 2 3 4 5 6 7 8))))

(define-primop (R_take [n 0] [arr all])
        (define cell-shape (vector-drop (rem-array-shape arr) 1))
        (rem-array (vector-append (vector (scalar->atom n)) cell-shape)
                   (vector-take (rem-array-data arr)
                                (* (for/product ([d cell-shape]) d)
                                   (scalar->atom n)))))
(module+ test
  (check-equal?
   (remora (R_take 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (array 0 1 2)
                  (array 3 4 5))))
  (check-equal?
   (remora ((rerank (0 1) R_take) 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (array 0 1)
                  (array 3 4)
                  (array 6 7)))))
(define-primop (R_take* [n 0] [arr all])
  (define cell-shape (vector-drop (rem-array-shape arr) 1))
  (R_box
   (rem-array (vector-append (vector (scalar->atom n)) cell-shape)
              (vector-take (rem-array-data arr)
                           (* (for/product ([d cell-shape]) d)
                              (scalar->atom n))))))
(module+ test
  (check-equal?
   (remora (R_take* 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (R_box (array (array 0 1 2)
                         (array 3 4 5)))))
  (check-equal?
   (remora ((rerank (0 1) R_take*) 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (R_box (array 0 1))
                  (R_box (array 3 4))
                  (R_box (array 6 7))))))

(define-primop (R_take-right [n 0] [arr all])
  (define cell-shape (vector-drop (rem-array-shape arr) 1))
  (rem-array (vector-append (vector (scalar->atom n)) cell-shape)
             (vector-take-right (rem-array-data arr)
                                (* (for/product ([d cell-shape]) d)
                                   (scalar->atom n)))))
(module+ test
  (check-equal?
   (remora (R_take-right 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (array 3 4 5)
                  (array 6 7 8))))
  (check-equal?
   (remora ((rerank (0 1) R_take-right) 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (array 1 2)
                  (array 4 5)
                  (array 7 8)))))
(define-primop (R_take-right* [n 0] [arr all])
  (define cell-shape (vector-drop (rem-array-shape arr) 1))
  (R_box
   (rem-array (vector-append (vector (scalar->atom n)) cell-shape)
              (vector-take-right (rem-array-data arr)
                                 (* (for/product ([d cell-shape]) d)
                                    (scalar->atom n))))))
(module+ test
  (check-equal?
   (remora (R_take-right* 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (R_box (array (array 3 4 5)
                         (array 6 7 8)))))
  (check-equal?
   (remora ((rerank (0 1) R_take-right*) 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (R_box (array 1 2))
                  (R_box (array 4 5))
                  (R_box (array 7 8))))))

(define-primop (R_drop [n 0] [arr all])
  (define cell-shape (vector-drop (rem-array-shape arr) 1))
  (rem-array (vector-append
              (vector (- (vector-ref (rem-array-shape arr) 0)
                         (scalar->atom n)))
              cell-shape)
             (vector-drop (rem-array-data arr)
                          (* (for/product ([d cell-shape]) d)
                             (scalar->atom n)))))
(module+ test
  (check-equal?
   (remora (R_drop 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (array 6 7 8))))
  (check-equal?
   (remora ((rerank (0 1) R_drop) 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (array 2)
                  (array 5)
                  (array 8)))))
(define-primop (R_drop* [n 0] [arr all])
  (define cell-shape (vector-drop (rem-array-shape arr) 1))
  (R_box
   (rem-array (vector-append
               (vector (- (vector-ref (rem-array-shape arr) 0)
                          (scalar->atom n)))
               cell-shape)
              (vector-drop (rem-array-data arr)
                           (* (for/product ([d cell-shape]) d)
                              (scalar->atom n))))))
(module+ test
  (check-equal?
   (remora (R_drop* 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (R_box (array (array 6 7 8)))))
  (check-equal?
   (remora ((rerank (0 1) R_drop*) 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (R_box (array 2))
                  (R_box (array 5))
                  (R_box (array 8))))))

(define-primop (R_drop-right [n 0] [arr all])
  (define cell-shape (vector-drop (rem-array-shape arr) 1))
  (rem-array (vector-append
              (vector (- (vector-ref (rem-array-shape arr) 0)
                         (scalar->atom n)))
              cell-shape)
             (vector-drop-right (rem-array-data arr)
                                (* (for/product ([d cell-shape]) d)
                                   (scalar->atom n)))))
(module+ test
  (check-equal?
   (remora (R_drop-right 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (array 0 1 2))))
  (check-equal?
   (remora ((rerank (0 1) R_drop-right) 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (array 0)
                  (array 3)
                  (array 6)))))
(define-primop (R_drop-right* [n 0] [arr all])
  (define cell-shape (vector-drop (rem-array-shape arr) 1))
  (R_box
   (rem-array (vector-append
               (vector (- (vector-ref (rem-array-shape arr) 0)
                          (scalar->atom n)))
               cell-shape)
              (vector-drop-right (rem-array-data arr)
                                 (* (for/product ([d cell-shape]) d)
                                    (scalar->atom n))))))
(module+ test
  (check-equal?
   (remora (R_drop-right* 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (R_box (array (array 0 1 2)))))
  (check-equal?
   (remora ((rerank (0 1) R_drop-right*) 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (R_box (array 0))
                  (R_box (array 3))
                  (R_box (array 6))))))

;;; Split an array into a list of cells of a given rank
(define (array->cell-list arr cell-rank)
  (define nat-cell-rank
    (if (>= cell-rank 0)
        cell-rank
        (+ (rem-array-rank arr) cell-rank)))
  (define frame-shape (vector-drop-right (rem-array-shape arr) nat-cell-rank))
  (define cell-count (for/product ([d frame-shape]) d))
  (define cell-shape (vector-take-right (rem-array-shape arr) nat-cell-rank))
  (define cell-size
    (for/product ([d cell-shape]) d))
  (for/list ([i cell-count])
    (rem-array cell-shape (subvector (rem-array-data arr)
                                     (* i cell-size)
                                     cell-size))))
(module+ test
  (check-equal?
   (array->cell-list (rem-array #(3 2 4) (for/vector ([i 24]) i)) 0)
   (for/list ([i 24]) (rem-array #() (vector i))))
  (check-equal?
   (array->cell-list (rem-array #(3 2 4) (for/vector ([i 24]) i)) 1)
   (for/list ([i 6]) (rem-array #(4) (for/vector ([j 4]) (+ j (* 4 i))))))
  (check-equal?
   (array->cell-list (rem-array #(3 2 4) (for/vector ([i 24]) i)) 2)
   (for/list ([i 3]) (rem-array #(2 4) (for/vector ([j 8]) (+ j (* 8 i)))))))

;;; Merge a list of cells into an array with the given frame shape
;;; If there are no cells in the list (i.e. empty frame), specify a cell shape
(define/contract (cell-list->array arrs frame-shape [opt-cell-shape #f])
  (->* (list? vector?)
       (vector?)
       rem-array?)
  (define cell-shape (or opt-cell-shape (rem-array-shape (first arrs))))
  (rem-array (vector-append frame-shape cell-shape)
             (apply vector-append (map rem-array-data arrs))))

; n is a positive number representing how deep to go to apply f to cells
; e.g. given [[1 2 3] [4 5 6]] n = 1 will apply f to [1 2 3] and [4 5 6],
; while n = 2 will apply it to 1, 2, 3, 4, 5, 6
; f has to produce elements of the same shape
(define-primop (R_map-cell [arr all] [f 0] [n 0])
  (define cells (array->cell-list arr (- n)))
  (define mapped-cells (map f cells))
  (if (empty? mapped-cells)
      (error)
      (cell-list->array mapped-cells
                    (length cells)
                    (rem-array-shape (car mapped-cells)))))

(define-primop (R_reverse [arr all])
  (if (zero? (vector-length (rem-array-shape arr)))
      arr
      (let ([length (vector-ref (rem-array-shape arr) 0)])
           (cell-list->array (reverse (array->cell-list arr -1))
                             (vector length)
                             (vector-drop (rem-array-shape arr) 1)))))
(module+ test
  (check-equal?
   (remora ((rerank (1) R_reverse)
            (alit (3 4) 0 1 2 3 4 5 6 7 8 9 10 11)))
   (remora (alit (3 4) 3 2 1 0 7 6 5 4 11 10 9 8)))
  (check-equal?
   (remora (R_reverse
            (alit (3 4) 0 1 2 3 4 5 6 7 8 9 10 11)))
   (remora (alit (3 4) 8 9 10 11 4 5 6 7 0 1 2 3))))

(define-primop (R_append [arr1 all] [arr2 all])
  (define cell-shape
    (cond [(and (vector-empty? (rem-array-shape arr1))
                (equal? (rem-array-shape arr1) (rem-array-shape arr2)))
           (vector)]
          [(equal? (vector-drop (rem-array-shape arr1) 1)
                (vector-drop (rem-array-shape arr2) 1))
           (vector-drop (rem-array-shape arr1) 1)]
          [else (error 'R_append "shape mismatch: ~v\t~v" arr1 arr2)]))
  (define top-level-size
    (if (and (vector-empty? (rem-array-shape arr1))
                (equal? (rem-array-shape arr1) (rem-array-shape arr2)))
        (vector 0)
        (vector (+ (vector-ref (rem-array-shape arr1) 0)
                               (vector-ref (rem-array-shape arr2) 0)))))
  (cell-list->array (append (array->cell-list arr1 -1)
                            (array->cell-list arr2 -1))
                    top-level-size
                    cell-shape))

(module+ test
  (check-equal?
   (remora (R_append
            (alit (4 3) 0 1 2 3 4 5 6 7 8 9 10 11)
            (alit (2 3) 20 30 40 50 60 70)))
   (remora (alit (6 3) 0 1 2 3 4 5 6 7 8 9 10 11 20 30 40 50 60 70)))
  (check-equal?
   (remora
    ((rerank (1 1) R_append)
     (alit (3 4) 0 1 2 3 4 5 6 7 8 9 10 11)
     (alit (3 2) 20 30 40 50 60 70)))
   (remora (alit (3 6) 0 1 2 3 20 30 4 5 6 7 40 50 8 9 10 11 60 70))))


;;; randomly choose items from an array without replacement
(define-primop (R_deal [count 0] [arr all])
  (define cell-shape (vector-drop (rem-array-shape arr) 1))
  (define cells (array->cell-list arr -1))
  (define shuffled (shuffle cells))
  (define first-cells (take shuffled
                            (vector-ref (rem-array-data count) 0)))
  (cell-list->array first-cells
                    (rem-array-data count)
                    cell-shape))
;;; randomly permute a list
(remora (def R_shuffle (fn ((xs all)) (R_deal (R_length xs) xs))))

(define (permutations items)
  (if (null? items) '(())
      (apply append
             (map (lambda (element)
            (map (lambda (permutation)
               (cons element permutation))
             (permutations (remove element items))))
          items))))

(define-primop (R_permutations [arr 1])
  (define len (vector-length (rem-array-data arr)))
  (define perms (permutations (vector->list (rem-array-data arr))))
  (define data (list->vector (flatten perms)))
  (rem-array (vector (length perms) len)
             data))

;;; Express a number in a given radix sequence
;;; TODO: permit +inf.0 so it can be used in outermost digit
(define (antibase radix num)
  (define (antibase-internal radix num)
    (cond [(empty? radix) (list num)]
          [else (cons (quotient num (for/product ([d radix]) d))
                      (antibase-internal
                       (sequence-tail radix 1)
                       (remainder num
                                  (for/product ([d radix]) d))))]))
  (rest (antibase-internal radix num)))

(define-primop (R_antibase [radix 1] [num 0])
  (define digits (antibase (vector->list (rem-array-data radix))
                           (vector-ref (rem-array-data num) 0)))
  (rem-array (vector (length digits))
             (list->vector digits)))
(module+ test
  (check-equal? (remora (R_antibase (alit (3) 3 2 4) (alit () 15)))
                (remora (alit (3) 1 1 3)))
  (check-equal? (remora (R_antibase (alit (3) 3 2 4) (alit (2) 15 25)))
                (remora (alit (2 3) 1 1 3 0 0 1)))
  (check-equal? (remora (R_antibase (alit (2 3) 2 5 1 3 2 4) (alit () 15)))
                (remora (alit (2 3) 1 0 0 1 1 3))))


(define (scan op init xs)
  (reverse
   (for/fold ([acc (list init) #;(list (sequence-ref xs 0))])
     ([elt xs #;(sequence-tail xs 1)])
     (cons (op (first acc) elt) acc))))

(define-primop (R_scan [op all] [init all] [xs all])
  (define input-items (array->cell-list xs -1))
  (define result-items
    (scan (λ (left right)
            (remora-apply op left right))
          init
          input-items))
  ; TODO: does this ever exec into the true branch
  (if (empty? result-items)
      (cell-list->array
       result-items
       (vector (length result-items))
       (rem-array-shape init))
      (cell-list->array
       result-items
       (vector (length result-items)))))
(module+ test
  (check-equal? (remora (R_scan - 0 (array 1 2 3)))
                (remora (array 0 -1 -3 -6)))
  (check-equal? (remora (R_scan + (array 0 0) (array (array 1 2)
                                                     (array 3 4))))
                (remora (array (array 0 0)
                               (array 1 2)
                               (array 4 6))))
  (check-equal? (remora ((rerank (all all 1) R_scan)
                         +
                         0 (array (array 1 2)
                                  (array 3 4))))
                (remora (array (array 0 1 3)
                               (array 0 3 7)))))
#|
#;(define (scan/zero op z all))
(define-primop (R_scan/zero [op all] [z all] [xs all])
  (R_scan op z xs))
(module+ test)

(define (open-scan/init op init xs)
  )
(define-primop (R_open-scan/init))
(module+ test)

(define (open-scan/zero))
(define-primop (R_open-scan/zero))
(module+ test)


(define (scan+final/zero))
(define-primop (R_scan+final/zero))
(module+ test)
|#


(define (scan+final/init op init xs)
  (define rev-res (for/fold ([acc (list init)])
                            ([elt xs])
                    (cons (op (first acc) elt) acc)))
  (racket-values (reverse rev-res) (car rev-res)))

(define-primop (R_scan+final/init [op all] [init all] [xs all])
  (define input-items (array->cell-list xs -1))
  (define-values (result-items final-res) (scan+final/init (λ (left right) (remora-apply op left right))
                                        init
                                        input-items))
  (racket-values (cell-list->array result-items (vector (length result-items))) final-res))
(module+ test)


(define (iscan+final/init op init xs)
  (define-values (res final) (scan+final/init op init xs))
  (racket-values (cdr res) final))
(define-primop (R_iscan+final/init [op all] [init all] [xs all])
  (define input-items (array->cell-list xs -1))
  (define-values (result-items final-res) (iscan+final/init (λ (left right) (remora-apply op left right))
                                                     init
                                                     input-items))
  (racket-values (cell-list->array result-items (vector (length result-items))) final-res))


(define (iscan op xs)
  (define xs-len (length xs))
  (if (equal? xs-len 0)
      xs
      (reverse
       (for/fold ([acc (list (car xs))])
                 ([elt (cdr xs)])
         (cons (op (car acc) elt) acc)))))

(define-primop (R_iscan [op all] [xs all])
  (define input-items (array->cell-list xs -1))
  (define res-items (iscan (λ (left right) (remora-apply op left right)) input-items))
  (cell-list->array res-items (vector (length res-items))))

(module+ test
  (check-equal? (remora (R_iscan + (array 1 2 3 4)))
                (remora (array 1 3 6 10)))
  (check-equal? (remora (R_iscan + (array (array 1 2) (array 3 4))))
                (remora (array (array 1 2) (array 4 6)))))

(define (iscan/init op init xs)
  (reverse
   (for/fold ([acc (list)])
             ([elt xs])
     (cons (op (if (empty? acc) init (car acc)) elt) acc))))


; TODO: op should also be able to accept 1 argument?
(define-primop (R_iscan/init [op all] [init all] [xs all])
  (define input-items (array->cell-list xs -1))
  (define res-items (iscan/init (λ (left right) (remora-apply op left right))
                           init input-items))
  (cell-list->array res-items (vector (length res-items))))

(module+ test
  (check-equal? (remora (R_iscan/init + 1 (array 1 2 3 4)))
                (remora (array 2 4 7 11)))
  (check-equal? (remora (R_iscan/init + (array 0 1)
                                      (array (array 1 2)
                                             (array 3 4)
                                             (array 5 6))))
                (remora (array (array 1 3)
                               (array 4 7)
                               (array 9 13)))))




;;; Interpret a digit list in a given radix
(define (base radix digits)
  ;; if radix is too short, extend by copying its first element
  (define padded-radix
    (if (> (length digits) (length radix))
        (append (for/list ([c (- (length digits)
                                 (length radix))])
                  (first radix))
                radix)
        radix))
  ;; if digits is too short, zero-extend it
  (define padded-digits
    (if (> (length radix) (length digits))
        (append (for/list ([c (- (length radix)
                                 (length digits))])
                  0)
                digits)
        digits))
  (for/sum ([place-value (reverse (scan * 1 (reverse
                                             (rest padded-radix))
                                        #;(cons 1 (reverse
                                                     (rest padded-radix)))))]
            [digit padded-digits])
    (* place-value digit)))
(define-primop (R_base [radix 1] [digits 1])
  (rem-array #() (vector
                  (base (vector->list (rem-array->vector radix))
                        (vector->list (rem-array->vector digits))))))
(module+ test
  (check-equal? (remora (R_base (alit (3) 3 2 4) (alit (3) 1 2 3)))
                (remora (alit () 19)))
  (check-equal? (remora (R_base (alit (1) 2) (alit (3) 1 0 1)))
                (remora (alit () 5)))
  (check-equal? (remora (R_base (alit (4) 7 24 60 60) (alit (3) 1 11 12)))
                (remora (alit () 4272))))

(define-primop (R_rotate [arr all] [shift 0])
  (define cells (array->cell-list arr -1))
  (define shift-atom (vector-ref (rem-array-data shift) 0))
  (define actual-shift
    (cond [(equal? 0 shift-atom) 0]
          [(negative? shift-atom) (modulo shift-atom (length cells))]
          [else (modulo shift-atom (length cells))]))
  (cell-list->array (append (drop cells actual-shift)
                            (take cells actual-shift))
                    (vector (length cells))
                    (vector-drop (rem-array-shape arr) 1)))
(module+ test
  (check-equal? (remora (R_rotate (array 1 2 3 4 5) 1))
                (remora (array 2 3 4 5 1)))
  (check-equal? (remora (R_rotate (array 1 2 3 4 5) -1))
                (remora (array 5 1 2 3 4)))
  (check-equal? (remora (R_rotate (array (array 0 1)
                                         (array 2 3)
                                         (array 4 5))
                                  1))
                (remora (array (array 2 3)
                               (array 4 5)
                               (array 0 1))))
  (check-equal? (remora ((rerank (1 0) R_rotate) (array (array 0 1)
                                                        (array 2 3)
                                                        (array 4 5))
                                  1))
                (remora (array (array 1 0)
                               (array 3 2)
                               (array 5 4))))
  (check-equal? (remora (R_rotate (array 0 1 2 3 4 5) (array 0 1 2 3)))
                (remora (array (array 0 1 2 3 4 5)
                               (array 1 2 3 4 5 0)
                               (array 2 3 4 5 0 1)
                               (array 3 4 5 0 1 2)))))

(define-primop (R_shape-of [arr all])
  (rem-array (vector (vector-length (rem-array-shape arr)))
             (rem-array-shape arr)))
(module+ test
  (check-equal? (remora (R_shape-of (alit (4 1 2) 3 6 2 3 5 2 3 4)))
                (remora (array 4 1 2))))

(define-primop (R_reshape [new-shape 1] [arr all])
  (define new-elt-count (for/product ([d (rem-array-data new-shape)]) d))
  (define old-elts (rem-array-data arr))
  (define old-elt-count (vector-length old-elts))
  (cond
    ;; reshaping empty -> empty
    [(and (zero? old-elt-count) (zero? new-elt-count))
     (rem-array (rem-array-data new-shape) (vector))]
    ;; reshaping empty -> nonempty (error case)
    [(zero? old-elt-count) (error 'R_reshape "cannot reshape empty array to nonempty")]
    ;; reshaping nonempty -> nonempty
    [else (rem-array
           (rem-array-data new-shape)
           (vector-take
            (apply vector-append
                   (for/list ([i (ceiling (/ new-elt-count old-elt-count))])
                             old-elts))
            new-elt-count))]))
(module+ test
  (check-equal? (remora (R_reshape (array 3 2)
                                   (alit (9) 1 2 3 4 5 6 7 8 9)))
                (remora (array (array 1 2)
                               (array 3 4)
                               (array 5 6))))
  (check-equal? (remora (R_reshape (array 3 2)
                                   (alit (5) 'a 'b 'c 'd 'e)))
                (remora (array (array 'a 'b)
                               (array 'c 'd)
                               (array 'e 'a))))
  (check-equal? (remora (R_reshape (array 0 3)
                                   (alit (2 0))))
                (remora (alit (0 3)))))
(define-primop (R_reshape* [new-shape 1] [arr all])
  (define new-elt-count (for/product ([d (rem-array-data new-shape)]) d))
  (define old-elts (rem-array-data arr))
  (define old-elt-count (vector-length old-elts))
  (R_box
   (rem-array
    (rem-array-data new-shape)
    (vector-take
     (apply vector-append
            (for/list ([i (ceiling (/ new-elt-count old-elt-count))])
                      old-elts))
     new-elt-count))))
(module+ test
  (check-equal? (remora (R_reshape* (array 3 2)
                                    (alit (9) 1 2 3 4 5 6 7 8 9)))
                (remora (R_box (array (array 1 2)
                                      (array 3 4)
                                      (array 5 6)))))
  (check-equal? (remora (R_reshape* (array 3 2)
                                    (alit (5) 'a 'b 'c 'd 'e)))
                (remora (R_box (array (array 'a 'b)
                                      (array 'c 'd)
                                      (array 'e 'a)))))
  (check-equal? (remora (R_reshape* (array (array 3 2)
                                           (array 2 3)
                                           (array 3 3))
                                    (alit (9) 1 2 3 4 5 6 7 8 9)))
                (remora (array (R_box (array (array 1 2)
                                             (array 3 4)
                                             (array 5 6)))
                               (R_box (array (array 1 2 3)
                                             (array 4 5 6)))
                               (R_box (array (array 1 2 3)
                                             (array 4 5 6)
                                             (array 7 8 9)))))))

; Axis is a permutation vector of numbers [0, n), where n is the rank of arr
; it defines the new order of indices 
(define-primop (R_transpose [arr all] [axis 1])
  (define shape (rem-array-shape arr))
  (define axis-list (vector->list (rem-array-data axis)))
  (define new-shape (list->vector (map (lambda (idx) (vector-ref shape idx)) axis-list)))
  (define old-indices-ranges (vector->list (vector-map (lambda (s) (rem-array (vector s) (list->vector (range 0 s)))) shape)))
  (define old-indices-ranges-arr (rem-array (vector) (vector old-indices-ranges)))
  (define old-indices (cart-product old-indices-ranges-arr))
  (define new-indices (map (lambda (old-idx)
                             (cell-list->array
                              (map (lambda (get-index-from)
                                    (R_index old-idx (rem-array (vector 1) (vector get-index-from))))
                                  axis-list)
                              (rem-array-shape old-idx)))
                           (array->cell-list old-indices 1)))
  (define new-data (list->vector (map (lambda (idx) (vector-ref (rem-array-data (R_index arr idx)) 0))
                                      new-indices)))
  (rem-array new-shape new-data))

(define-primop (R_iota [shape 1])
  (define size (for/product ([d (rem-array-data shape)]) d))
  (rem-array (rem-array-data shape)
             (for/vector ([i size]) i)))
(module+ test
  (check-equal? (remora (R_iota (array 4)))
                (remora (array 0 1 2 3)))
  (check-equal? (remora (R_iota (array 4 3)))
                (remora (array (array 0 1 2)
                               (array 3 4 5)
                               (array 6 7 8)
                               (array 9 10 11)))))
(define-primop (R_iota* [shape 1])
  (define size (for/product ([d (rem-array-data shape)]) d))
  (R_box (rem-array (rem-array-data shape)
                    (for/vector ([i size]) i))))
(module+ test
  (check-equal? (remora (R_iota* (array 4)))
                (remora (R_box (array 0 1 2 3))))
  (check-equal? (remora (R_iota* (array 4 3)))
                (remora (R_box (array (array 0 1 2)
                                      (array 3 4 5)
                                      (array 6 7 8)
                                      (array 9 10 11)))))
  (check-equal? (remora (R_iota* (array (array 4)
                                        (array 3))))
                (remora (array (R_box (array 0 1 2 3))
                               (R_box (array 0 1 2))))))


(define (list-nub xs [already-seen '()])
  (cond [(empty? xs) '()]
        [(member (first xs) already-seen) (list-nub (rest xs) already-seen)]
        [else (cons (first xs)
                    (list-nub (rest xs)
                              (cons (first xs) already-seen)))]))
(define-primop (R_nub [arr all])
  (define cells
    (list-nub (array->cell-list arr -1)))
  (cell-list->array cells
                    (vector (length cells))
                    (vector-drop (rem-array-shape arr) 1)))
(module+ test
  (check-equal? (remora (R_nub (array 1 4 2 3 1 9 2 3 8 2)))
                (remora (array 1 4 2 3 9 8)))
  (check-equal? (remora (R_nub (array (array 1 4)
                                      (array 2 3)
                                      (array 1 9)
                                      (array 2 3)
                                      (array 8 2))))
                (remora (array (array 1 4)
                               (array 2 3)
                               (array 1 9)
                               (array 8 2)))))

(define (list-nub-sieve xs [already-seen '()])
  (cond [(empty? xs) '()]
        [(member (first xs) already-seen)
         (cons #f (list-nub-sieve (rest xs) already-seen))]
        [else (cons #t (list-nub-sieve (rest xs)
                                       (cons (first xs) already-seen)))]))
(define-primop (R_nub-sieve [arr all])
  (define cells
    (list-nub-sieve (array->cell-list arr -1)))
  (rem-array (vector (length cells))
             (list->vector cells)))
(module+ test
  (check-equal? (remora (R_nub-sieve (array 1 4 2 3 1 9 2 3 8 2)))
                (remora (array #t #t #t #t #f #t #f #f #t #f)))
  (check-equal? (remora (R_nub-sieve (array (array 1 4)
                                            (array 2 3)
                                            (array 1 9)
                                            (array 2 3)
                                            (array 8 2))))
                (remora (array #t #t #t #f #t))))

(define-primop (R_ravel [arr all])
  (rem-array (vector (for/product ([d (rem-array-shape arr)]) d))
             (rem-array-data arr)))
(module+ test
  (check-equal? (remora (R_ravel (array 1 2 3 4)))
                (remora (array 1 2 3 4)))
  (check-equal? (remora (R_ravel (array (array 1 2)
                                        (array 3 4))))
                (remora (array 1 2 3 4))))

(define-primop (R_itemize [arr all])
  (rem-array (vector-append #(1) (rem-array-shape arr))
             (rem-array-data arr)))
(module+ test
  (check-equal? (remora (R_itemize (array 1 2 3 4)))
                (remora (array (array 1 2 3 4))))
  (check-equal? (remora ((rerank (0) R_itemize) (array 1 2 3 4)))
                (remora (array (array 1)
                               (array 2)
                               (array 3)
                               (array 4))))
  (check-equal? (remora (R_itemize (array (array 1 2)
                                          (array 3 4))))
                (remora (array (array (array 1 2)
                                      (array 3 4))))))

(define-primop (R_length [arr all])
  (scalar (vector-ref (rem-array-shape arr) 0)))
(module+ test
  (check-equal? (remora (R_length (alit (4 3) 0 1 2 3 4 5 6 7 8 9 10 11)))
                (remora 4))
  (check-equal? (remora ((rerank (1) R_length)
                         (alit (4 3) 0 1 2 3 4 5 6 7 8 9 10 11)))
                (remora (array 3 3 3 3))))


(define-primop (R_equal [xs all] [ys all])
  (scalar (equal? xs ys)))
(module+ test
  (check-equal? (remora (R_equal (array 1 1 1 1) 1))
                (remora #f))
  (check-equal? (remora ((rerank (0 0) R_equal) (array 1 1 1 1) 1))
                (remora (array #t #t #t #t)))
  (check-equal? (remora ((rerank (0 0) R_equal) (array 0 1 2 3) 1))
                (remora (array #f #t #f #f))))

;;; print a whole array structure (don't just lift and print atoms one-by-one)
;;; TODO: need a version of this that lets caller specify an output port
(define R_show
  (rem-array
   #()
   (vector
    (Rλ ([arr all]) (rem-array #() (vector (print arr)))))))

(define-primop (R_build-array (f 0) (shape 1))
  (remora (f (R_iota shape))))

;;; read a whole array structure
(define-primop (R_read [port 0])
  (list->array (read (vector-ref (rem-array-data port) 0))))

;;; right fold a list of Remora arrays using a Remora function array
(define (rem-foldr op base arrays)
  (cond [(empty? arrays) base]
        [else (remora-apply op
                            (first arrays)
                            (rem-foldr op base (rest arrays)))]))
(define-primop (R_foldr [op all] [base all] [arr all])
  (rem-foldr op base (array->cell-list arr -1)))
(module+ test
  (check-equal? (remora (R_foldr + 0 (array 1 2 3 4)))
                (remora 10))
  (check-equal? (remora (R_foldr - 0 (array 1 2 3 4)))
                (remora -2))
  (check-equal? (remora (R_foldr (array + -) 0 (array 1 2 3 4)))
                (remora (array 10 -2))))

;;; left fold a list of Remora arrays using a Remora function array
(define (rem-foldl op base arrays)
  (cond [(empty? arrays) base]
        [else
         (rem-foldl op (remora-apply op base (first arrays)) (rest arrays))
         #;(remora-apply op
                         (first arrays)
                         (rem-foldr op base (rest arrays)))]))
(define-primop (R_foldl [op all] [base all] [arr all])
  (rem-foldl op base (array->cell-list arr -1)))
(module+ test
  (check-equal? (remora (R_foldl + 0 (array 1 2 3 4)))
                (remora 10))
  (check-equal? (remora (R_foldl - 0 (array 1 2 3 4)))
                (remora -10))
  (check-equal? (remora (R_foldl (array + -) 0 (array 1 2 3 4)))
                (remora (array 10 -10))))


;;; reduce a vector (not list!) of Remora arrays using a Remora function array
;;; note: reduce assumes associativity and has no "base" value
(define (rem-reduce op base arrays)
  (cond [(= 0 (vector-length arrays)) base]
        [(= 1 (vector-length arrays)) (vector-ref arrays 0)]
        [else (define-values (left right)
                (vector-split-at arrays (ceiling (/ (vector-length arrays) 2))))
              (remora-apply op (rem-reduce op base left) (rem-reduce op base right))]))
(define-primop (R_reduce [op all] [base all] [arr all])
  (rem-reduce op base (list->vector (array->cell-list arr -1))))
(module+ test
  (check-equal? (remora (R_reduce + 0 (array 1 2 3 4)))
                (remora 10))
  (check-equal? (remora (R_reduce (array + *) (array 0 1) (array 1 2 3 4)))
                (remora (array 10 24))))



(define (reduce-n op base arr n)
  (define n-val (vector-ref (rem-array-data n) 0))
  (when (< (vector-length (rem-array-shape arr)) n-val)  (error "Trying to reduce more times than there are dimensions"))
  (cond [(zero? n-val) arr]
        [else
         (define collapsed-one-layer (remora ((rerank (0 all 1) R_reduce) op base arr)))
         (remora (R_reduce-n op base collapsed-one-layer (sub1 n)))]))
; reduces n times starting with the deep-most dimensions
(define-primop (R_reduce-n [op all] [base all] [arr all] [n 0])
  (reduce-n op base arr n))

(define (reduce-n-2 op base arr n)
  (define n-val (vector-ref (rem-array-data n) 0))
  (when (< (vector-length (rem-array-shape arr)) n-val)  (error "Trying to reduce more times than there are dimensions"))
  (cond [(zero? n-val) arr]
        [else
         (define collapsed-one-layer (remora ((rerank (0 1 2) R_reduce) op base arr)))
         (remora (R_reduce-n-2 op base collapsed-one-layer (sub1 n)))]))
; reduces n times starting with the deep-most dimensions
(define-primop (R_reduce-n-2 [op all] [base all] [arr all] [n 0])
  (reduce-n-2 op base arr n))

;;; Extract a box's contents
;;; Applying this to an array of boxes risks producing result cells with
;;; mismatching shapes.
(define-primop (R_unsafe-unbox [b 0])
  (if (rem-box? (scalar->atom b))
      (rem-box-contents (scalar->atom b))
      (printf "oops, b is ~s\n" b)))


(define-primop (R_filter [bools 1] [items all])
  (define cell-shape (vector-drop (rem-array-shape items) 1))
  (define old-cells (array->cell-list items -1))
  (define choices (rem-array-data bools))
  (define new-cells
    (for/list ([b choices] [c old-cells] #:when b) c))
  (cell-list->array new-cells
                    (vector (length new-cells))
                    cell-shape))
(module+ test
  (check-equal? (remora (R_filter (array #t #t #f #t #f #t)
                                  (array 1 2 #t 3 #f 4)))
                (remora (array 1 2 3 4)))
  (check-equal? (remora (R_filter (array #f #f #t #f #t #f)
                                  (array 1 2 #t 3 #f 4)))
                (remora (array #t #f)))
  (check-equal? (remora ((rerank (all 1) R_filter)
                         (array #t #f #t)
                         (array (array 1 2 3)
                                (array 4 5 6))))
                (remora (array (array 1 3)
                               (array 4 6))))
  (check-equal? (remora (R_filter (array #t #f #f #t)
                                  (array (array 1 2 3 0)
                                         (array 0 1 3 2)
                                         (array 2 4 5 9)
                                         (array 6 6 6 0))))
                (remora (array (array 1 2 3 0)
                                    (array 6 6 6 0)))))
(define-primop (R_filter* [bools 1] [items all])
  (define cell-shape (vector-drop (rem-array-shape items) 1))
  (define old-cells (array->cell-list items -1))
  (define choices (rem-array-data bools))
  (define new-cells
    (for/list ([b choices] [c old-cells] #:when b) c))
  (R_box
   (cell-list->array new-cells
                     (vector (length new-cells))
                     cell-shape)))
(module+ test
  (check-equal? (remora (R_filter* (array #t #t #f #t #f #t)
                                   (array 1 2 #t 3 #f 4)))
                (remora (R_box (array 1 2 3 4))))
  (check-equal? (remora (R_filter* (array #f #f #t #f #t #f)
                                   (array 1 2 #t 3 #f 4)))
                (remora (R_box (array #t #f))))
  (check-equal? (remora ((rerank (all 1) R_filter)
                         (array #t #f #t)
                         (array (array 1 2 3)
                                (array 4 5 6))))
                (remora (array (array 1 3)
                               (array 4 6))))
  (check-equal? (remora (R_filter* (array #t #f #f #t)
                                   (array (array 1 2 3 0)
                                          (array 0 1 3 2)
                                          (array 2 4 5 9)
                                          (array 6 6 6 0))))
                (remora (R_box (array (array 1 2 3 0)
                                    (array 6 6 6 0))))))

(define-primop (R_index [arr all] [idx 1])
  ; check if the idx has correct number of elements
  (define shape-vec (rem-array-shape arr))
  (define idx-vec (rem-array-data idx))
  (if (> (vector-length idx-vec) (vector-length shape-vec))
      (error "invalid index given, index: " idx-vec ", shape of the array: " shape-vec)
      #f)
  ; check if any index is out of bounds
  (if (not (zero? (vector-count (lambda (dim i) (>= i dim)) (vector-take shape-vec (vector-length idx-vec)) idx-vec)))
      (error "One of the indices is out of bounds, index: " idx ", shape of array: " shape-vec)
      #f)
  (if (not (zero? (vector-count negative? idx-vec)))
      (error "One of the indices is negative, index: " idx)
      #f)
  (define arr-shape (remora (R_shape-of arr)))
  (define split-at-idx (vector-length idx-vec))
  (define res-shape (vector-drop shape-vec split-at-idx))

  (define index-coef-vector (remora (R_take split-at-idx (R_reverse (R_scan * 1 (R_reverse (R_behead arr-shape)))))))
  (define res-size (foldr * 1 (vector->list res-shape)))
  (define index-of-elem (remora (R_reduce + 0 (* index-coef-vector idx))))
  (define res (vector-take (vector-drop (rem-array-data arr) (vector-ref (rem-array-data index-of-elem) 0))
                           res-size))
  (rem-array res-shape res))

(module+ test
  (check-equal? (remora (R_index (R_iota (array 3 4 5)) (array 1 2 4)))
                (remora 34))
  (check-equal? (remora (R_index (R_iota (array 10)) (array 8)))
                (remora 8))
  (check-exn exn:fail? (lambda () (remora (R_index (array 1 2 3) (array )))))
  (check-exn exn:fail? (lambda () (remora (R_index (array 1 2 3) (array 2 3)))))
  (check-exn exn:fail? (lambda () (remora (R_index (array 1 2 3) (array -1)))))
  (check-exn exn:fail? (lambda () (remora (R_index (array 1 2 3) (array 3))))))



(define-primop (R_array-set [arr all] [idx 1] [new-val all])
  (define shape-vec (rem-array-shape arr))
  (define idx-vec (rem-array-data idx))
  (when (> (vector-length idx-vec) (vector-length shape-vec))
    (error "Wrong index"))
  (define new-val-shape (rem-array-shape new-val))
  (define split-at-idx (vector-length idx-vec))
  (unless (equal? (vector-drop shape-vec split-at-idx) new-val-shape)
    (error "New value wrong shape"))
  (define arr-data (rem-array-data arr))
  (define new-val-data (rem-array-data new-val))
  ; calc the 'flat' index in the result arr
  (define index-shape (remora (R_take split-at-idx (R_shape-of arr))))
  (define index-coef-vector (remora (R_reverse (R_scan * 1 (R_reverse (R_behead index-shape))))))
  (define index-of-elem-remora (remora (R_reduce + 0 (* index-coef-vector idx))))
  (define index-of-elem (vector-ref (rem-array-data index-of-elem-remora) 0))
  (vector-copy! arr-data index-of-elem new-val-data)
  (rem-array shape-vec arr-data))

; produces a vector of numbers [start, end) with given step
; negative step values are not supported
; if start = end, then returns [start]
(define-primop (R_range [start 0] [end 0] [step 0])
  (define start-num (vector-ref (rem-array-data start) 0))
  (define end-num (vector-ref (rem-array-data end) 0))
  (cond [(< end-num start-num)      (remora (array))]
        [(equal? end-num start-num) (remora (array start))]
        [else                       (remora (+ start (* step (R_iota (array (exact-ceiling (/ (- end start) step)))))))]))


(define (vector-flatten nest-vec)
  (cond [(not (vector? nest-vec)) nest-vec]
        [(vector-empty? nest-vec) nest-vec]
        [(vector? (vector-ref nest-vec 0)) (apply vector-append (vector->list (vector-map vector-flatten nest-vec)))]
        [else nest-vec]))

(define (dimensional-slice nest-vec offset-vec size-vec)
  (define offset-len (vector-length offset-vec))
  (define size-len (vector-length size-vec))
  (cond [(and (zero? offset-len) (zero? size-vec)) (error "")]
        [(not (equal? offset-len size-len))        (error "offset and size have different sizes")]
        [(equal? 1 offset-len)                     (vector-take (vector-drop nest-vec (vector-ref offset-vec 0)) (vector-ref size-vec 0))]
        [else                                      (vector-map
                                                    (lambda (cell) (dimensional-slice cell (vector-drop offset-vec 1) (vector-drop size-vec 1)))
                                                    (vector-take (vector-drop nest-vec (vector-ref offset-vec 0)) (vector-ref size-vec 0)))]))

; arr - array to take a slice from
; offset - offset of the slice, number of elements has to be the same as rank
; slice-size - size of each slice's dimesion, number of elements has to be the same as rank.
; Throws an error if offset + slice-size is larger than corresponding dimensions
(define-primop (R_slice [arr all] [offset 1] [slice-size 1])
  (define shape-vec (rem-array-shape arr))
  (define offset-vec (rem-array-data offset))
  (define slice-size-vec (rem-array-data slice-size))
  (when (< (vector-length shape-vec) (vector-length offset-vec))
    (error "Offset has the wrong length"))
  (when (< (vector-length shape-vec) (vector-length slice-size-vec))
    (error "Slice size has the wrong length"))
  (define nested-vec-arr (array->nest-vector arr))
  (define res-nested-vec (dimensional-slice nested-vec-arr offset-vec slice-size-vec))
  (rem-array slice-size-vec (vector-flatten res-nested-vec)))



; same as slice, but if the slice goes out of bound of arr (on any side, through origin being
; negative or shape + origin > size of arr), the rest is filled with scalar values fill
; returns an array of shape slice-shape
(define-primop (R_slice/fill [arr all] [origin 1] [slice-shape 1] [fill 0])
  (define filled-new-arr (remora (R_reshape slice-shape fill)))
  (define old-arr-shape (remora (R_shape-of arr)))
  (define max-of-2 (remora (fn ((a 0) (b 0)) (R_select (> a b) a b))))
  (define min-of-2 (remora (fn ((a 0) (b 0)) (R_select (< a b) a b))))
  (define old-arr-slice-origin (remora (max-of-2 ((fn ((_ 0)) 0) slice-shape) origin)))
  (define old-arr-slice-end (remora (sub1 (min-of-2 (+ slice-shape origin) old-arr-shape))))
  (define old-arr-slice-origin-list (R_array->nest-list old-arr-slice-origin))
  (define old-arr-slice-end-list (R_array->nest-list old-arr-slice-end))
  (define idx-ranges (remora (map (fn ((origin 0) (end 0)) (R_range origin (add1 end) 1))
                                  old-arr-slice-origin-list
                                  old-arr-slice-end-list)))
  (define all-idx (remora (R_cartesian-product idx-ranges)))
  ; there is prolly a better version of doing this, maybe using reduce, but good enough for now
  (remora (R_foldr (fn ((idx 1) (new-arr all)) (R_array-set new-arr (- idx origin) (R_index arr idx)))
                   filled-new-arr
                   all-idx)))


; vectors-scalar is a scalar rem array with a list of index ranges inside
(define (cart-product vectors-scalar)
  (define vectors (vector-ref (rem-array-data vectors-scalar) 0))
  (define vector-num (length vectors))
  (cond [(zero? vector-num) (remora (array))]
        [(equal? vector-num 1) (remora (remora-apply (rerank (0) R_itemize) (car vectors)))]
        [else
         (define itemized-head (remora (remora-apply (rerank (0) R_itemize) (car vectors))))
         (define cart-product-rest (cart-product (scalar (cdr vectors))))
         (remora (def (append-to-rest (n 1)) ((rerank (1 1) R_append) n cart-product-rest)))
         (define array-res (append-to-rest itemized-head))
         (define formatted-res (array->cell-list array-res -2))
         (cell-list->array formatted-res
                           (vector (length formatted-res))
                           (vector vector-num))]))

; accept 2d-array where each row is an array of elements for cartesian product
; '([1 2] [3 4] [5 6]) -> [[1 3 5] [1 3 6] [1 4 5] [1 4 6] [2 3 5] [2 3 6] [2 4 5] [2 4 6]]
(define-primop (R_cartesian-product [vectors 0])
  (cart-product vectors))

(define-primop (R_select [bool 0] [a all] [b all])
  (if (scalar->atom bool) a b))
(module+ test
  (check-equal? (remora (R_select #t (array 1 2 3) (array 4 5 6)))
                (remora (array 1 2 3)))
  (check-equal? (remora (R_select #f (array 1 2 3) (array 4 5 6)))
                (remora (array 4 5 6)))
  (check-equal? (remora (R_select (array #t #f) (array 1 2 3) (array 4 5 6)))
                (remora (array (array 1 2 3) (array 4 5 6))))
  (check-equal? (remora ((rerank (0 0 0) R_select) (array #t #f #t)
                                                   (array 1 2 3)
                                                   (array 4 5 6)))
                (remora (array 1 5 3))))

;;; Enable "sliding window" computation over a vector. Subsequences of specified
;;; length are aligned along the major axis for easy folding.
(define-primop (R_window [length 0] [arr all])
  (R_rotate arr (R_iota (R_itemize length))))
(module+ test
  (check-equal? (remora (R_window 3 (array 1 2 3 4 5 6 7 8 9 10)))
                (remora (array (array 1 2 3 4 5 6 7 8 9 10)
                               (array 2 3 4 5 6 7 8 9 10 1)
                               (array 3 4 5 6 7 8 9 10 1 2))))
  (check-equal? (remora ((rerank (0 1) R_window) 3
                                                 (array (array 1 2 3 4 5)
                                                        (array 6 7 8 9 10))))
                (remora [array [array [array 1 2 3 4 5]
                                      [array 2 3 4 5 1]
                                      [array 3 4 5 1 2]]
                               [array [array 6 7 8 9 10]
                                      [array 7 8 9 10 6]
                                      [array 8 9 10 6 7]]])))



;;; Convert a Racket nested list into a Remora array
(define-primop (R_list->array [lst 0])
  (list->array (scalar->atom lst)))

;;; Convert a Remora array to a nested Racket list containing its atoms
(define-primop (R_array->nest-list [arr all])
        (scalar (array->nest-list arr)))
(module+ test
  (check-equal? (remora (R_array->nest-list 4))
                (remora 4))
  (check-equal? (remora (R_array->nest-list (array 1 2 3 4 5 6)))
                (remora '(1 2 3 4 5 6)))
  (check-equal? (remora (R_array->nest-list (array (array 1 2 3 4 5 6))))
                (remora '((1 2 3 4 5 6))))
  (check-equal? (remora (R_array->nest-list (array (array 1 2 3)
                                              (array 4 5 6))))
                (remora '((1 2 3) (4 5 6))))
  (check-equal? (remora (R_array->nest-list (array (array 1 2)
                                              (array 3 4)
                                              (array 5 6))))
                (remora '((1 2) (3 4) (5 6)))))
;;; Convert a Remora array to a nested Racket vector containing its atoms
(define-primop (R_array->nest-vector [arr all])
  (scalar (array->nest-vector arr)))
(module+ test
  (check-equal? (remora (R_array->nest-vector 4))
                (remora 4))
  (check-equal? (remora (R_array->nest-vector (array 1 2 3 4 5 6)))
                (remora '#(1 2 3 4 5 6)))
  (check-equal? (remora (R_array->nest-vector (array (array 1 2 3 4 5 6))))
                (remora '#(#(1 2 3 4 5 6))))
  (check-equal? (remora (R_array->nest-vector (array (array 1 2 3)
                                              (array 4 5 6))))
                (remora '#(#(1 2 3) #(4 5 6))))
  (check-equal? (remora (R_array->nest-vector (array (array 1 2)
                                              (array 3 4)
                                              (array 5 6))))
                (remora '#(#(1 2) #(3 4) #(5 6)))))
;;; Convert a Remora array to a flattened Racket list containing its atoms
(define-primop (R_array->flat-list [arr all])
  (scalar (vector->list (rem-array-data arr))))
(module+ test
  (check-equal? (remora (R_array->flat-list 4))
                (remora '(4)))
  (check-equal? (remora (R_array->flat-list (array 1 2 3 4 5 6)))
                (remora '(1 2 3 4 5 6)))
  (check-equal? (remora (R_array->flat-list (array (array 1 2 3 4 5 6))))
                (remora '(1 2 3 4 5 6)))
  (check-equal? (remora (R_array->flat-list (array (array 1 2 3)
                                                   (array 4 5 6))))
                (remora '(1 2 3 4 5 6)))
  (check-equal? (remora (R_array->flat-list (array (array 1 2)
                                                   (array 3 4)
                                                   (array 5 6))))
                (remora '(1 2 3 4 5 6))))
;;; Convert a Remora array to a flattened Racket vector containing its atoms
(define-primop (R_array->flat-vector [arr all])
  (scalar (rem-array-data arr)))
(module+ test
  (check-equal? (remora (R_array->flat-vector 4))
                (remora '#(4)))
  (check-equal? (remora (R_array->flat-vector (array 1 2 3 4 5 6)))
                (remora '#(1 2 3 4 5 6)))
  (check-equal? (remora (R_array->flat-vector (array (array 1 2 3 4 5 6))))
                (remora '#(1 2 3 4 5 6)))
  (check-equal? (remora (R_array->flat-vector (array (array 1 2 3)
                                                     (array 4 5 6))))
                (remora '#(1 2 3 4 5 6)))
  (check-equal? (remora (R_array->flat-vector (array (array 1 2)
                                                     (array 3 4)
                                                     (array 5 6))))
                (remora '#(1 2 3 4 5 6))))

;;; Convert a Racket string to a Remora vector containing its characters
(remora (def R_string->array
          (fn ((str 0)) (R_list->array (string->list str)))))

;;; Construct a Racket string from a Remora vector of characters
(remora (def R_array->string
          (fn ((arr 1)) (list->string (R_array->nest-list arr)))))

;;; Search an associative array (vector of length-2 lists)
;;; We won't be able to type this without a cast that effectively assumes at
;;; least one result is found.
(remora
 (def R_lookup
   (fn ((needle all) (haystack 1))
     (unbox results (R_filter
                     ((fn ((table-entry 0)) (R_equal needle
                                                     (first table-entry)))
                      haystack)
                     haystack)
       (second (R_head results))))))
;;; Like R_lookup but with an alternative value to use in case the desired key
;;; is not present
(remora
 (def R_lookup+
   (fn ((needle all) (haystack 1) (alternate all))
     (unbox results (R_filter
                     ((fn ((table-entry 0)) (R_equal needle
                                                     (first table-entry)))
                      haystack)
                     haystack)
       (second (R_head (R_append results [array
                                          (list 'missing alternate)])))))))
;;; Like R_lookup but returning every matching value
(remora
 (def R_lookup*
   (fn ((needle all) (haystack 1) (alternate all))
     (unbox results (R_filter
                     ((fn ((table-entry 0)) (R_equal needle
                                                     (first table-entry)))
                      haystack)
                     haystack)
       (second (R_head (R_append results [array
                                          (list 'missing alternate)])))))))

;;; Construct and destruct pairs
#|
(define-primop (R_pair [new-car all] [new-cdr all]) (scalar (cons new-car new-cdr)))
(define-primop (R_fst [pair 0]) (car (scalar->atom pair)))
(define-primop (R_snd [pair 0]) (cdr (scalar->atom pair)))

(define R_+
 (rem-array #() (vector (rem-scalar-proc (lambda (x y) (d+ x y)) 2))))

(define R_-
 (rem-array #() (vector (rem-scalar-proc (lambda (x y) (d- x y)) 2))))

(define R_*
 (rem-array #() (vector (rem-scalar-proc (lambda (x y) (d* x y)) 2))))

(define R_/
 (rem-array #() (vector (rem-scalar-proc (lambda (x y) (d/ x y)) 2))))

(define R_sqrt
 (rem-array #() (vector (rem-scalar-proc (lambda (x) (dsqrt x)) 1))))

(define R_exp
 (rem-array #() (vector (rem-scalar-proc (lambda (x) (dexp x)) 1))))

(define R_log
 (rem-array #() (vector (rem-scalar-proc (lambda (x) (dlog x)) 1))))

(define R_expt
 (rem-array #() (vector (rem-scalar-proc (lambda (x) (dexpt x)) 1))))

(define R_sin
 (rem-array #() (vector (rem-scalar-proc (lambda (x) (dsin x)) 1))))

(define R_cos
 (rem-array #() (vector (rem-scalar-proc (lambda (x) (dcos x)) 1))))

(define R_atan
 (rem-array #() (vector (rem-scalar-proc (lambda (x y) (datan x y)) 2))))

(define R_=
 (rem-array #() (vector (rem-scalar-proc (lambda (x y) (d= x y)) 2))))

(define R_<
 (rem-array #() (vector (rem-scalar-proc (lambda (x y) (d< x y)) 2))))

(define R_>
 (rem-array #() (vector (rem-scalar-proc (lambda (x y) (d> x y)) 2))))

(define R_<=
 (rem-array #() (vector (rem-scalar-proc (lambda (x y) (d<= x y)) 2))))

(define R_>=
 (rem-array #() (vector (rem-scalar-proc (lambda (x y) (d>= x y)) 2))))

(define R_zero?
 (rem-array #() (vector (rem-scalar-proc (lambda (x) (dzero? x)) 1))))

(define R_positive?
 (rem-array #() (vector (rem-scalar-proc (lambda (x) (dpositive? x)) 1))))

(define R_negative
 (rem-array #() (vector (rem-scalar-proc (lambda (x) (dnegative? x)) 1))))

(define R_real?
 (rem-array #() (vector (rem-scalar-proc (lambda (x) (dreal? x)) 1))))
|#