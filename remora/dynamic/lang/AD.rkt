#lang racket/base

;;; A fork of https://github.com/qobi/R6RS-AD/blob/master/AD.ss

(require "semantics.rkt"
	 racket)

(provide d+
	 d-
	 d*
	 d/
	 dsqrt
	 dexp
	 dlog
	 dexpt
	 dsin
	 dcos
	 datan
	 d=
	 d<
	 d>
	 d<=
	 d>=
	 dzero?
	 dpositive?
	 dnegative?
	 dreal?
	 R_j*
	 R_*j)

(define *e* 0)

(define <_e <)

(define-struct bundle (epsilon primal tangent))

(define-struct tape
 (epsilon
  primal
  factors
  tapes
  (fanout #:mutable)
  (cotangent #:mutable)))

(define (for-each-n f n)
 (let loop ((i 0)) (when (< i n) (f i) (loop (+ i 1)))))

(define (vector-for-each f v . &rest)
 (for-each-n
  (lambda (i)
   (apply f (vector-ref v i) (map (lambda (v) (vector-ref v i)) &rest)))
  (vector-length v)))

(define (new-tape epsilon primal factors tapes)
 (make-tape epsilon primal factors tapes 0 0))

(define (tapify x) (new-tape *e* x '() '()))

(define (lift-real->real f df/dx)
 (letrec ((self (lambda (x)
		 (cond ((bundle? x)
			(make-bundle (bundle-epsilon x)
				     (self (bundle-primal x))
				     (d* (df/dx (bundle-primal x))
					 (bundle-tangent x))))
		       ((tape? x)
			(new-tape (tape-epsilon x)
				  (self (tape-primal x))
				  (list (df/dx (tape-primal x)))
				  (list x)))
		       (else (f x))))))
  self))

(define (lift-real*real->real f df/dx1 df/dx2)
 (letrec ((self
	   (lambda (x1 x2)
	    (cond
	     ((bundle? x1)
	      (cond
	       ((bundle? x2)
		(cond
		 ((<_e (bundle-epsilon x1)
		       (bundle-epsilon x2))
		  (make-bundle (bundle-epsilon x2)
			       (self x1 (bundle-primal x2))
			       (d* (df/dx2 x1 (bundle-primal x2))
				   (bundle-tangent x2))))
		 ((<_e (bundle-epsilon x2)
		       (bundle-epsilon x1))
		  (make-bundle (bundle-epsilon x1)
			       (self (bundle-primal x1) x2)
			       (d* (df/dx1 (bundle-primal x1) x2)
				   (bundle-tangent x1))))
		 (else
		  (make-bundle
		   (bundle-epsilon x1)
		   (self (bundle-primal x1) (bundle-primal x2))
		   (d+ (d* (df/dx1 (bundle-primal x1)
				   (bundle-primal x2))
			   (bundle-tangent x1))
		       (d* (df/dx2 (bundle-primal x1)
				   (bundle-primal x2))
			   (bundle-tangent x2)))))))
	       ((tape? x2)
		(if (<_e (bundle-epsilon x1) (tape-epsilon x2))
		    (new-tape (tape-epsilon x2)
			      (self x1 (tape-primal x2))
			      (list (df/dx2 x1 (tape-primal x2)))
			      (list x2))
		    (make-bundle (bundle-epsilon x1)
				 (self (bundle-primal x1) x2)
				 (d* (df/dx1 (bundle-primal x1) x2)
				     (bundle-tangent x1)))))
	       (else (make-bundle (bundle-epsilon x1)
				  (self (bundle-primal x1) x2)
				  (d* (df/dx1 (bundle-primal x1) x2)
				      (bundle-tangent x1))))))
	     ((tape? x1)
	      (cond
	       ((bundle? x2)
		(if (<_e (tape-epsilon x1) (bundle-epsilon x2))
		    (make-bundle (bundle-epsilon x2)
				 (self x1 (bundle-primal x2))
				 (d* (df/dx2 x1 (bundle-primal x2))
				     (bundle-tangent x2)))
		    (new-tape (tape-epsilon x1)
			      (self (tape-primal x1) x2)
			      (list (df/dx1 (tape-primal x1) x2))
			      (list x1))))
	       ((tape? x2)
		(cond
		 ((<_e (tape-epsilon x1) (tape-epsilon x2))
		  (new-tape (tape-epsilon x2)
			    (self x1 (tape-primal x2))
			    (list (df/dx2 x1 (tape-primal x2)))
			    (list x2)))
		 ((<_e (tape-epsilon x2) (tape-epsilon x1))
		  (new-tape (tape-epsilon x1)
			    (self (tape-primal x1) x2)
			    (list (df/dx1 (tape-primal x1) x2))
			    (list x1)))
		 (else
		  (new-tape (tape-epsilon x1)
			    (self (tape-primal x1) (tape-primal x2))
			    (list (df/dx1 (tape-primal x1) (tape-primal x2))
				  (df/dx2 (tape-primal x1) (tape-primal x2)))
			    (list x1 x2)))))
	       (else (new-tape (tape-epsilon x1)
			       (self (tape-primal x1) x2)
			       (list (df/dx1 (tape-primal x1) x2))
			       (list x1)))))
	     (else
	      (cond ((bundle? x2)
		     (make-bundle (bundle-epsilon x2)
				  (self x1 (bundle-primal x2))
				  (d* (df/dx2 x1 (bundle-primal x2))
				      (bundle-tangent x2))))
		    ((tape? x2)
		     (new-tape (tape-epsilon x2)
			       (self x1 (tape-primal x2))
			       (list (df/dx2 x1 (tape-primal x2)))
			       (list x2)))
		    (else (f x1 x2))))))))
  self))

(define (fold f l)
 (let loop ((l (cdr l)) (c (car l)))
  (if (null? l) c (loop (cdr l) (f c (car l))))))

(define (lift-real^n->real f df/dx1 df/dx2)
 (lambda xs
  (if (null? xs) (f) (fold (lift-real*real->real f df/dx1 df/dx2) xs))))

(define (lift-real^n+1->real f df/dx df/dx1 df/dx2)
 (lambda xs
  (cond ((null? xs) (f))
	((null? (cdr xs)) ((lift-real->real f df/dx) (car xs)))
	(else (fold (lift-real*real->real f df/dx1 df/dx2) xs)))))

(define (primal* x)
 (cond ((bundle? x) (primal* (bundle-primal x)))
       ((tape? x) (primal* (tape-primal x)))
       (else x)))

(define (lift-real^n->boolean f) (lambda xs (apply f (map primal* xs))))

(define d+ (lift-real^n->real + (lambda (x1 x2) 1) (lambda (x1 x2) 1)))

(define d- (lift-real^n+1->real
	    - (lambda (x) -1) (lambda (x1 x2) 1) (lambda (x1 x2) -1)))

(define d* (lift-real^n->real * (lambda (x1 x2) x2) (lambda (x1 x2) x1)))

(define d/ (lift-real^n+1->real
	    /
	    (lambda (x) (d- (d/ (d* x x))))
	    (lambda (x1 x2) (d/ x2))
	    (lambda (x1 x2) (d- (d/ x1 (d* x2 x2))))))

(define dsqrt (lift-real->real sqrt (lambda (x) (d/ (d* 2 (dsqrt x))))))

(define dexp (lift-real->real exp (lambda (x) (dexp x))))

(define dlog (lift-real->real log (lambda (x) (d/ x))))

(define dexpt
 (lift-real*real->real expt
		       (lambda (x1 x2) (d* x2 (dexpt x1 (d- x2 1))))
		       (lambda (x1 x2) (d* (dlog x1) (dexpt x1 x2)))))

(define dsin (lift-real->real sin (lambda (x) (dcos x))))

(define dcos (lift-real->real cos (lambda (x) (d- (dsin x)))))

(define (datan . xs)
 (cond ((null? xs) (apply atan xs))
       ((null? (cdr xs)) (datan (car xs) 1))
       ((null? (cdr (cdr xs)))
	((lift-real*real->real
	  atan
	  (lambda (x1 x2) (d/ x2 (d+ (d* x1 x1) (d* x2 x2))))
	  (lambda (x1 x2) (d/ (d- x1) (d+ (d* x1 x1) (d* x2 x2)))))
	 (car xs)
	 (cadr xs)))
       (else (apply atan xs))))

(define d= (lift-real^n->boolean =))

(define d< (lift-real^n->boolean <))

(define d> (lift-real^n->boolean >))

(define d<= (lift-real^n->boolean <=))

(define d>= (lift-real^n->boolean >=))

(define dzero? (lift-real^n->boolean zero?))

(define dpositive? (lift-real^n->boolean positive?))

(define dnegative? (lift-real^n->boolean negative?))

;;;\needswork: Make a version of number?.
(define dreal? (lift-real^n->boolean real?))

(define (map-independent2 f x x-tangent)
 ;;\needswork: We don't support structs.
 (cond ((and (eq? x #t) (eq? x-tangent #t)) #t)
       ((and (eq? x #f) (eq? x-tangent #f)) #f)
       ((and (dreal? x) (dreal? x-tangent)) (f x x-tangent))
       ((and (rem-array? x)
	     (rem-array? x-tangent)
	     (equal? (rem-array-shape x) (rem-array-shape x-tangent)))
	(rem-array
	 (rem-array-shape x)
	 (vector-map (lambda (x x-tangent) (map-independent2 f x x-tangent))
		     (rem-array-data x)
		     (rem-array-data x-tangent))))
       ((and (rem-proc? x) (rem-proc? x-tangent))
	(error 'map-independent2 "cannot handle functions ~v\t~v" x x-tangent))
       (else (error 'map-independent2 "nonconformant ~v\t~v" x x-tangent))))

(define (map-dependent-forward f y-forward)
 ;;\needswork: We don't support structs.
 (cond ((eq? y-forward #t) #t)
       ((eq? y-forward #f) #f)
       ((dreal? y-forward) (f y-forward))
       ((rem-array? y-forward)
	(rem-array
	 (rem-array-shape y-forward)
	 (vector-map (lambda (y-forward) (map-dependent-forward f y-forward))
		     (rem-array-data y-forward))))
       ((rem-proc? y-forward)
	(error 'map-dependent-forward "cannot handle functions ~v" y-forward))
       (else (error 'map-dependent-forward "unrecognized data type ~v"
		    y-forward))))

(define (forward-mode f x x-tangent)
 ;;\needswork: We don't support what the AD community calls tangent vector
 ;;            mode.
 (set! *e* (+ *e* 1))
 (let* ((y-forward
	 (apply-rem-array
	  f
	  (map-independent2 (lambda (x x-tangent)
			     (make-bundle *e* x x-tangent))
			    x
			    x-tangent)))
	(y (map-dependent-forward
	    (lambda (y-forward)
	     (if (and (bundle? y-forward)
		      (not (<_e (bundle-epsilon y-forward) *e*)))
		 (bundle-primal y-forward)
		 y-forward))
	    y-forward))
	(y-tangent
	 (map-dependent-forward
	  (lambda (y-forward)
	   (if (and (bundle? y-forward)
		    (not (<_e (bundle-epsilon y-forward) *e*)))
	       (bundle-tangent y-forward)
	       0))
	  y-forward)))
  (set! *e* (- *e* 1))
  ;;\needswork: We don't support returning both y and y-tangent.
  y-tangent))

(define (determine-fanout! tape)
 (set-tape-fanout! tape (+ (tape-fanout tape) 1))
 (when (= (tape-fanout tape) 1)
  (for-each determine-fanout! (tape-tapes tape))))

(define (initialize-cotangent! tape)
 (set-tape-cotangent! tape 0)
 (set-tape-fanout! tape (- (tape-fanout tape) 1))
 (when (zero? (tape-fanout tape))
  (for-each initialize-cotangent! (tape-tapes tape))))

(define (reverse-sweep! cotangent tape)
 (set-tape-cotangent! tape (d+ (tape-cotangent tape) cotangent))
 (set-tape-fanout! tape (- (tape-fanout tape) 1))
 (when (zero? (tape-fanout tape))
  (let ((cotangent (tape-cotangent tape)))
   (for-each (lambda (factor tape) (reverse-sweep! (d* cotangent factor) tape))
	     (tape-factors tape)
	     (tape-tapes tape)))))

(define (map-independent f x)
 ;;\needswork: We don't support structs.
 (cond ((eq? x #t) #t)
       ((eq? x #f) #f)
       ((dreal? x) (f x))
       ((rem-array? x)
	(rem-array
	 (rem-array-shape x)
	 (vector-map (lambda (x) (map-independent f x)) (rem-array-data x))))
       ((rem-proc? x) (error 'map-independent "cannot handle functions ~v" x))
       (else (error 'map-independent "unrecognized data type ~v" x))))

(define (map-independent-reverse f x-reverse)
 ;;\needswork: We don't support structs.
 (cond ((eq? x-reverse #t) #t)
       ((eq? x-reverse #f) #f)
       ((dreal? x-reverse) (f x-reverse))
       ((rem-array? x-reverse)
	(rem-array
	 (rem-array-shape x-reverse)
	 (vector-map (lambda (x-reverse) (map-independent-reverse f x-reverse))
		     (rem-array-data x-reverse))))
       ((rem-proc? x-reverse)
	(error 'map-independent-reverse "cannot handle functions ~v" x-reverse))
       (else (error 'map-independent-reverse "unrecognized data type ~v"
		    x-reverse))))

(define (map-dependent-reverse f y-reverse)
 ;;\needswork: We don't support structs.
 (cond ((eq? y-reverse #t) #t)
       ((eq? y-reverse #f) #f)
       ((dreal? y-reverse) (f y-reverse))
       ((rem-array? y-reverse)
	(rem-array
	 (rem-array-shape y-reverse)
	 (vector-map (lambda (y-reverse) (map-dependent-reverse f y-reverse))
		     (rem-array-data y-reverse))))
       ((rem-proc? y-reverse)
	(error 'map-dependent-reverse "cannot handle functions ~v" y-reverse))
       (else (error 'map-dependent-reverse "unrecognized data type ~v"
		    y-reverse))))

(define (for-each-dependent1! f y-reverse)
 ;;\needswork: We don't support structs.
 (cond ((eq? y-reverse #t) #f)
       ((eq? y-reverse #f) #f)
       ((dreal? y-reverse) (f y-reverse))
       ((rem-array? y-reverse)
	(vector-for-each (lambda (y-reverse) (for-each-dependent1! f y-reverse))
			 (rem-array-data y-reverse)))
       ((rem-proc? y-reverse)
	(error 'for-each-dependent1! "cannot handle functions ~v" y-reverse))
       (else (error 'for-each-dependent1! "unrecognized data type ~v"
		    y-reverse))))

(define (for-each-dependent2! f y-reverse y-cotangent)
 ;;\needswork: We don't support structs.
 (cond ((and (eq? y-reverse #t) (eq? y-cotangent #t)) #f)
       ((and (eq? y-reverse #f) (eq? y-cotangent #f)) #f)
       ((and (dreal? y-reverse) (dreal? y-cotangent))
	(f y-reverse y-cotangent))
       ((and (rem-array? y-reverse)
	     (rem-array? y-cotangent)
	     (equal? (rem-array-shape y-reverse) (rem-array-shape y-cotangent)))
	(vector-for-each (lambda (y-reverse y-cotangent)
			  (for-each-dependent2! f y-reverse y-cotangent))
			 (rem-array-data y-reverse)
			 (rem-array-data y-cotangent)))
       ((and (rem-proc? y-reverse) (rem-proc? y-cotangent))
	(error 'for-each-dependent2! "cannot handle functions ~v\t~v"
	       y-reverse y-cotangent))
       (else (error 'for-each-dependent2! "nonconformant ~v\t~v"
		    y-reverse y-cotangent))))

(define (reverse-mode f x y-cotangent)
 ;;\needswork: We don't support providing the y-cotangents (potentially
 ;;            incrementally) after computing the primal in the forward
 ;;            sweep.
 (set! *e* (+ *e* 1))
 (let* ((x-reverse (map-independent tapify x))
	(y-reverse (apply-rem-array f x-reverse)))
  (for-each-dependent1!
   (lambda (y-reverse)
    (when (and (tape? y-reverse) (not (<_e (tape-epsilon y-reverse) *e*)))
     (determine-fanout! y-reverse)))
   y-reverse)
  (for-each-dependent1!
   (lambda (y-reverse)
    (when (and (tape? y-reverse) (not (<_e (tape-epsilon y-reverse) *e*)))
     (initialize-cotangent! y-reverse)))
   y-reverse)
  (for-each-dependent1!
   (lambda (y-reverse)
    (when (and (tape? y-reverse) (not (<_e (tape-epsilon y-reverse) *e*)))
     (determine-fanout! y-reverse)))
   y-reverse)
  (for-each-dependent2!
   (lambda (y-reverse y-cotangent)
    (when (and (tape? y-reverse) (not (<_e (tape-epsilon y-reverse) *e*)))
     (reverse-sweep! y-cotangent y-reverse)))
   y-reverse
   y-cotangent)
  (let ((x-cotangent (map-independent-reverse tape-cotangent x-reverse))
	(y (map-dependent-reverse
	    (lambda (y-reverse)
	     (if (and (tape? y-reverse)
		      (not (<_e (tape-epsilon y-reverse) *e*)))
		 (tape-primal y-reverse)
		 y-reverse))
	    y-reverse)))
   (set! *e* (- *e* 1))
   ;;\needswork: We don;t support returning both y and x-cotangent.
   x-cotangent)))

(define R_j* (rem-proc forward-mode '(0 all all)))

(define R_*j (rem-proc reverse-mode '(0 all all)))
