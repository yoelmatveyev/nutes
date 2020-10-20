(in-package :cl-nutes)

;; Defining the main structures:

; For the actual virtual machines
; The 'special' field is reserved for IO and other later extensions

(defstruct tape
  width
  power
  range
  position
  flex
  vector
  length
  halted
  special
  counter
  )

; For the assembled code

(defstruct program
  id
  direction
  entry
  data
  code
  labels
  first
  last
  length
  )

; Constants for standard tapes according to the system architecture

(if (> (floor (log most-positive-fixnum 3)) 36)
    (defconstant st-width 36)
    (defconstant st-width 18))

 (defconstant st-width 36)

(defconstant st-power (expt 3 st-width))
(defconstant st-range (/ (1- st-power) 2))

(defparameter width st-width)

; Setting the default word width of flexible machines

(defun create-tape
    (length &key (pos 0) (count 0) (flex nil) (width width))
  (let ((p)(r))
    (setf flex (if (> width 39) t flex)
	  p (expt 3 width)
	  r (/ (1- p) 2))
    (make-tape
     :width width
     :power p
     :range r
     :position pos
     :flex flex
     :vector (if flex
		 (make-array length)
		 (if (<= width 9)
		     (make-array length :element-type '(signed-byte 16))
		     (if (<= width 19)
			 (make-array length :element-type '(signed-byte 32))
			 (make-array length :element-type 'fixnum))))
     :length length
     :halted nil
     :special nil
     :counter count)))

;; We convert integers to balanced ternary numbers of a given range

(defun coerce-width (x &key (width st-width))
  (let*
      ((power (expt 3 width))
       (range (/ (1- power) 2))
       (n (mod x power)))
    (if (> n range)
	(- n power) n)))

;; Sign sum function. Not used in the machine, but may be useful elsewhere.

(defun sign-sum (x y)
  (signum (+ (signum x)(signum y))))

;; Our 'CPU'

(declaim (inline one-step signum tape-position tape-vector tape-counter tape-halted tape-special mod))

(defun one-step (tape)
  (declare (optimize (speed 3) (safety 0)(debug 0)))
(let
      ((l (tape-length tape))
       (p (tape-position tape))
       (v (tape-vector tape))
       (power (tape-power tape))
       (range (tape-range tape))
       (p+ 0) (p- 0) (j 0) (new-jmp 0) (m+ 0) (m- 0) (a+ 0) (a- 0) (sub 0) (sign 0))
    (declare (type fixnum l p p+ p- j new-jmp m+ m- a+ a- sub sign power range))   
    (if (plusp p)
	(if (= 1 (- l p))
	  (setf p+ (aref v 0) p- (aref v (1- p)))
	  (setf p- (aref v (1- p)) p+ (aref v (1+ p))))
	(setf p- (aref v (1- l)) p+ (aref v (1+ p))))
    (setf j (mod (+ (aref v p) p) l)
	  m+ (mod (+ p p+) l)
	  m- (mod (+ p p-) l)
	  a+ (aref v m+)
	  a- (aref v m-)
	  sub (let ((n (- a+ a-)))
	      (if (> n range) (- n power)
		  (if (< n (- range)) (+ power n)
		      n)))
	  sign (+ (signum a+)(signum a-)))
    (if (zerop sign)
	(setf new-jmp (aref v j))
	(if (plusp sign)
	     (setf new-jmp
	       (if (plusp j)
		   (if (= 1 (- l j))
		       (aref v 0)
		       (aref v (1+ j)))
		   (aref v (1+ j))))
	     (setf new-jmp
		(if (plusp j)
		    (if (= 1 (- l j))
			(aref v (1- j))
			(aref v (1- j)))
		    (aref v (1- l))))))
  (incf (tape-counter tape))
  (if (and (zerop new-jmp) (zerop sign))
      (progn
	(setf (tape-halted tape) t
	      (tape-special tape)
	       (if (> (abs a-)(abs a+))
		a-
		(if (< (abs a-)(abs a+))
		    a+
		    0)))
	(run-io-engine tape))
      (progn
	(setf (tape-position tape) (mod (+ p new-jmp) l)
	      (aref v m+) sub
	      (aref v m-) (- sub))
	t))))

;; Flexible word width version not optimized for fixnums

(defun one-step-flex (tape)
  (declare (optimize (speed 3) (safety 0)(debug 0)))
(let*
    ((l (tape-length tape))
     (p (tape-position tape))
     (v (tape-vector tape))
     (power (tape-power tape))
     (range (tape-range tape))
     p+ p- j pj j- j+ j0 m- m+ a+ a- sub sign)
  (if (plusp p)
      (if (= 1 (- l p))
	  (setf p+ (aref v 0) p- (aref v (1- p)))
	  (setf p- (aref v (1- p)) p+ (aref v (1+ p))))
      (setf p- (aref v (1- l)) p+ (aref v (1+ p))))
  (setf j (mod (+ (aref v p) p) l)
	m+ (mod (+ p p+) l)
	m- (mod (+ p p-) l)
	a+ (aref v m+)
	a- (aref v m-)
	sub (let ((n (- a+ a-)))
	      (if (> n range) (- n power)
		  (if (< n (- range)) (+ power n)
		      n)))
	sign (+ (signum a+)(signum a-)))
  (if (zerop sign)
      (setf new-jmp (aref v j))
      (if (plusp sign)
	  (setf new-jmp
		(if (plusp j)
		    (if (= 1 (- l j))
			(aref v 0)
			(aref v (1+ j)))
		    (aref v (1+ j))))
	  (setf new-jmp
		(if (plusp j)
		    (if (= 1 (- l j))
			(aref v (1- j))
			(aref v (1- j)))
		    (aref v (1- l))))))
  (incf (tape-counter tape))
  (if (and (zerop new-jmp) (zerop sign))
      (progn
	(setf (tape-halted tape) t
	      (tape-special tape)
	      (if (> (abs a-)(abs a+))
		  a-
		  (if (< (abs a-)(abs a+))
		      a+
		      0)))
	(run-io-engine tape))
      (progn
	(setf (tape-position tape) (mod (+ p new-jmp) l)
	      (aref v m+) sub
	      (aref v m-) (- sub))
	t))))

;; Write your program using the tools provides by instructions.lisp and feed it to the tape

(defun load-tape (tape prg &key (offset 0) (middle nil))
  (let ((list))
    (if middle (setf offset (+ (floor (/ (tape-length tape) 2)) offset)))
    (if
     (listp prg)
     (setf list prg)
     (setf list (append (program-data prg) (program-code prg))
	   (tape-position tape) (mod offset (tape-length tape))
	   offset (+ offset (program-first prg))))
  (loop for p from 0 to (1- (length list))
     do (setf (elt (tape-vector tape)
		   (mod (+ p offset) (tape-length tape)))
              (coerce-width (pop list) :width (tape-width tape))))))

;; Running our tape

; By a certain number of steps

(defun step-tape (tape &optional (n 1))
  (if (< n 1) (setf n 1))
  (loop for x from 1 to n unless (tape-halted tape) do
       (if (tape-flex tape)
	   (one-step-flex tape)
	   (one-step tape))))

; Indefinitely

(defun run-tape (tape)
  (if (tape-flex tape)
      (loop do
	   (one-step-flex tape)
	 while (not (tape-halted tape)))
      (loop do
	   (one-step tape)
	 while (not (tape-halted tape)))))
