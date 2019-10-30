(load "ternary-print.lisp")
(load "input-output.lisp")
(load "assembler.lisp")
(load "instructions.lisp")
(load "examples.lisp")

(defstruct tape
  position
  initial
  vector
  length
  halted
  special
  counter
  )

;; The 'special' field in tapes is reserved for IO and other later extensions

(defun create-tape
    (length &optional (position 0) (counter 0))
		   (make-tape
		    :position position
		    :initial 0
		    :vector (make-array length)
		    :length length
		    :halted nil
		    :special nil
		    :counter counter))

;; We convert integers to balanced ternary numbers of a given range
;; Set your own width here

(defparameter width 36)
(defparameter power (expt 3 width))
(defparameter range (/ (1- power) 2))

(defun coerce-width (x)
  (let ((n (mod x power)))
    (if (> n range)
	(- n power) n)))

;; Sign sum function

(defun sign-sum (x y)
  (signum (+ (signum x)(signum y))))

;; Our main 'CPU'

(defun one-step (tape)
  (declare (optimize speed (safety 0)(debug 0)))
  (let*
      ((p (tape-position tape))
       (v (tape-vector tape))
       (l (tape-length tape))
       (p+ (elt v (mod (1- p ) l)))
       (p- (elt v (mod (1+ p ) l)))
       (j (elt v (mod p l)))
       (pj (+ p j))
       (j- (elt v (mod (1- pj ) l)))
       (j+ (elt v (mod (1+ pj ) l)))
       (j0 (elt v (mod pj l)))
       (a+ (elt v (mod (+ p p+) l)))
       (a- (elt v (mod (+ p p-) l)))
       (sub (coerce-width (- a+ a-)))
       (sign (sign-sum a+ a-)))
    (incf (tape-counter tape))
    (setf (elt v (mod (+ p p+) l)) sub)
    (setf (elt v (mod (+ p p-) l)) (- sub))
    (case sign
      (1 (setf (tape-position tape) (mod (+ p j+) l)))
      (0 (setf (tape-position tape) (mod (+ p j0) l)))
      (-1 (setf (tape-position tape) (mod (+ p j-) l))))
    (if (and (zerop j0) (zerop sub))
	(progn (setf (tape-halted tape) t)
	       (setf (tape-special tape) a+)
	       )
	t)))

;; Write your program using the tools provides by the file insructions.lisp and feed it to the tape

(defun load-tape (tape prg &optional (offset 0))
  (let ((list))
  (if
   (not (listp prg))
	(progn
	  (setf list (append (program-data prg) (program-code prg)))
	  (setf (tape-position tape) offset)
	  (setf offset (+ offset (program-first prg))))
	  (setf list prg))
  (loop for p from 0 to (1- (length list))
     do (setf (elt (tape-vector tape)
		   (mod (+ p offset) (tape-length tape)))
              (coerce-width (pop list))))))

;; Running our tape

(defun run-tape (tape)
  (loop do
     (one-step tape)
     while (not (tape-halted tape)))
  (if (tape-halted tape)
	(format t "~&Tape halted~&")))

