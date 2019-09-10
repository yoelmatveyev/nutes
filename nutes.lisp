(defstruct tape
  width
  position
  initial
  vector
  length
  halted
  special
  )

;; The 'special' field in tapes is reserved for IO and other later extensions

(defmacro initiate-tape
    (name length width &optional (initial 0) (special 27))
      `(setf ,name
       	(make-tape :width ,width :position ,initial :initial ,initial
		   :vector ,(make-array length) :length ,length
		   :halted nil :special ,special)))

;; We convert integers to balanced ternary numbers of a given range

(defun coerce-width (x &optional (width 9))
  (let* ((power (expt 3 width))
	 (range (/ (1- power) 2))
	 (n (mod x power)))
    (if (> n range)
	(- n power) n)))

;; We feed a list of commands to a tape

(defun program-tape (name list &optional (offset 0))
  (loop for p from 0 to (1- (length list))
      do (setf (elt (tape-vector name)
		    (mod (+ p offset) (tape-length name)))
	       (coerce-width (pop list) (tape-width name)))))

;; Generator of modulo sum functions

(defun mod+ (m)
  (lambda (&rest x) (mod (apply #'+ x) m)))

;; Ternary analog of XOR

(defun ternary-xor (x y)
  (1- (mod (+ 1 (signum x)(signum y))3)))

;; Our main 'CPU'

(defun one-step (tape)
  (let*
      ((p (tape-position tape))
       (v (tape-vector tape))
       (l (tape-length tape))
       (p+ (elt v (mod (- p 1) l)))
       (p- (elt v (mod (+ p 1) l)))
       (j (elt v (mod p l)))
       (j- (elt v (mod (+ p j -1) l)))
       (j+ (elt v (mod (+ p j 1) l)))
       (j0 (elt v (mod (+ p j) l)))
       (a+ (elt v (mod (+ p p+) l)))
       (a- (elt v (mod (+ p p-) l)))
       (sub (coerce-width (- a+ a-) (tape-width tape)))
       (sign (ternary-xor a+ a-)))
    (setf (elt v (mod (+ p p+) l)) sub)
    (setf (elt v (mod (+ p p-) l)) (- sub))
    (case sign
      (1 (setf (tape-position tape) (mod (+ p j+) l)))
      (0 (setf (tape-position tape) (mod (+ p j0) l)))
      (-1 (setf (tape-position tape) (mod (+ p j-) l))))
	(if (and (zerop j0) (zerop sub))
	    (progn (setf (tape-halted tape) t)
	    (list (elt v (mod (- p 1) l))
	    (elt v (mod (+ p 1) l)))
	    (print "Program halted")))))


;; Running our tape

(defun run-tape (tape &optional (steps (expt 3 15)))
(one-step tape)
  (if (tape-halted tape)
      (progn
	    (terpri)(print "Tape halted"))
      (if (> steps 0) (run-tape tape (1- steps))
      (progn
	(terpri)
	(print "Hm.. no termination after so many steps? Giving up for now")))))
	  



  
