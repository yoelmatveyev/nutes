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

; Constants for standard 36-trit tapes

(defconstant st-width 36)
(defconstant st-power (expt 3 st-width))
(defconstant st-range (/ (1- st-power) 2))

(defparameter width st-width)

; Setting the default word width of flexible machines

(defun create-tape
    (length &key (pos 0) (count 0) (flex nil) (width width))
  (let ((p)(r))
    (setf flex (if (/= width st-width) t flex)
	  width (if flex width st-width)
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
		 (make-array length :element-type 'fixnum))
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
       (p+ 0) (p- 0) (j 0) (pj 0) (j- 0) (j+ 0) (j0 0) (m+ 0) (m- 0) (a+ 0) (a- 0) (sub 0) (sign 0))
    (declare (type fixnum p l p+ p- j pj j- j+ j0 a+ a- sub sign))   
    (setf j (aref v p))
    (if (plusp p)
	(if (= 1 (- l p))
	    (setf p+ (aref v 0) p- (aref v (1- p)))
	    (setf p- (aref v (1- p)) p+ (aref v (1+ p))))
	(setf p- (aref v (1- l)) p+ (aref v (1+ p))))
    (setf pj (mod (+ p j) l))
    (if (plusp pj)
	(if (= 1 (- l pj))
	    (setf j+ (aref v 0) j- (aref v (1- pj)))
	    (setf j- (aref v (1- pj)) j+ (aref v (1+ pj))))
	(setf j- (aref v (1- l)) j+ (aref v (1+ pj))))
    (setf j0 (aref v pj)
	  m+ (mod (+ p p+) l)
	  m- (mod (+ p p-) l)
	  a+ (aref v m+)
	  a- (aref v m-)
	  sub (let ((n (- a+ a-)))
		(if (> n st-range) (- n st-power)
		    (if (< n (- st-range)) (+ st-power n)
			n)))
	  sign (+ (signum a+)(signum a-)))
    (incf (tape-counter tape))
    (if (and (zerop j0) (zerop sub))
	(progn (setf (tape-halted tape) t
		     (tape-special tape) a+)
	       (input-output tape))
	(progn
	  (if (plusp sign)
	      (setf (tape-position tape) (mod (+ p j+) l))
	      (if (minusp sign)
		  (setf (tape-position tape) (mod (+ p j-) l))
		  (setf (tape-position tape) (mod (+ p j0) l))))
	  (setf (aref v m+) sub
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
       (p+) (p-) (j) (pj) (j-) (j+) (j0) (m-) (m+) (a+) (a-) (sub) (sign))
  (setf j (aref v p))
  (if (plusp p)
      (if (= 1 (- l p))
	  (setf p+ (aref v 0) p- (aref v (1- p)))
	  (setf p- (aref v (1- p)) p+ (aref v (1+ p))))
      (setf p- (aref v (1- l)) p+ (aref v (1+ p))))
  (setf pj (mod (+ p j) l))
  (if (plusp pj)
      (if (= 1 (- l pj))
	  (setf j+ (aref v 0) j- (aref v (1- pj)))
	  (setf j- (aref v (1- pj)) j+ (aref v (1+ pj))))
      (setf j- (aref v (1- l)) j+ (aref v (1+ pj))))
  (setf j0 (aref v pj)
	m+ (mod (+ p p+) l)
	m- (mod (+ p p-) l)
	a+ (aref v m+)
	a- (aref v m-)
	sub (let ((n (- a+ a-)))
	      (if (> n range) (- n power)
		  (if (< n (- range)) (+ power n)
		      n)))
	sign (+ (signum a+)(signum a-)))
  (incf (tape-counter tape))
  (if (and (zerop j0) (zerop sub))
      (progn (setf (tape-halted tape) t
		   (tape-special tape) a+)
	     (input-output tape))
      (progn
	(if (plusp sign)
	    (setf (tape-position tape) (mod (+ p j+) l))
	    (if (minusp sign)
		(setf (tape-position tape) (mod (+ p j-) l))
		(setf (tape-position tape) (mod (+ p j0) l))))
	(setf (aref v m+) sub
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
	   (tape-position tape) offset
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
	   (one-step tape)))
  (if (tape-halted tape)
	(format t "~&Tape halted~&")))

; Indefinitely

(defun run-tape (tape)
  (if (tape-flex tape)
      (loop do
	   (one-step-flex tape)
	 while (not (tape-halted tape)))
      (loop do
	   (one-step tape)
	 while (not (tape-halted tape))))
  (if (tape-halted tape)
	(format t "~&Tape halted~&")))
