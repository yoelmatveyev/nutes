(in-package :cl-nutes)

;; Interrupt and IO engine

(defparameter op-pointer (expt 3 9))

(defparameter 3^6 (expt 3 6))

;; Decode and encode operands for an interrupt

(defun op-split (n &key (width 36))
  (when (<= width 3) (setf width 3)) 
  (coerce (3n-digits n :b (expt 3 (floor width 3)) :l 3) 'vector))

(defun op-gen (x &key (width 36))
  (let* ((w (floor width 3))
	 (b (expt 3 w)))
    (setf x (mapcar (lambda (_) (coerce-width _ :width w)) x)) 
    (reduce (lambda (h l)(+ (* h b) l)) 
	    (coerce x 'list))))

;; Decode and encode strings of trytes

(defun trytes-chars (n)
      (let ((str)
	    (d (3n-split n :b 3^6)))
	(loop for x from 0 to (1- (length d)) do
	     (if (nth x d)
		 (progn
		   (push (code-char (abs (nth x d))) str)
		   (if (< (nth x d) 0)(push (code-char #x0305) str)))))
      		 (coerce str 'string)))

(defun chars-trytes (str &key neg)
  (let ((codes)(res))
    (setf codes (map 'list (lambda (x) (char-code x)) str))
    (setf res (reduce
     (lambda (h l)(+ (* h 3^6) l))
     codes))(if neg (- res) res)))

;; Find values of relative addresses on a tape

(defun eval-addr (tape spl)
  (let ((pos (tape-position tape))
	(l (tape-length tape)))
    (if (numberp spl)
	(elt (tape-vector tape) (mod (+ pos spl) l))
	(map 'vector
	     (lambda (x)
	       (elt (tape-vector tape) (mod (+ pos x) l)))
	     (coerce spl 'vector)))))

;; Decode opcodes

(defun decode-op (n)
  (let ((split (op-split n :width (length (3n-split n))))
	(flags)
	(dir (signum n))(param)(op)(halt))
    (if (zerop dir) (setf halt t)
	  (setf flags
		(cdr (mapcar (lambda (_) (* _ dir)) (3n-digits (elt split 0))))
		param (* (elt split 1) dir)
		op (* (elt split 2) dir)))
    (coerce (list dir flags param op halt) 'vector)))
  
;; Input

(defun convert-minus (s &key (b 27))
  (let ((res '()) (sign
	      (if (equal (subseq s 0 1) "-")
		  (progn
		    (setf s (subseq s 1 (length s)))
		    -1)
		  1))
	(flip 1))
    (loop for x across s do
	 (if (eq x #\|)
	     (setf flip -1)
	     (progn
	       (push (* (parse-integer (string x) :radix 14) flip) res)
	       (setf flip 1))))
    (op-gen (reverse (mapcar (lambda (x)(* x sign)) res)) :width (* (log b 3)3 ))))

;; Input (unchecked)

(defun ternary-input (&key (mode 0))
  (let ((l (read-line)))
    (case mode
      (-1 (parse-integer l))
      (-2 (convert-minus l :b 3))
      (1 (convert-minus l :b 9))
      (4 (convert-minus l :b 27))
      (t (chars-trytes l)))))
  
;; Output

 (defun ternary-output (n &key (mode 0))
   (case mode
     (-1 (print n))
     (-2 (ternary-print n :b 3))
     (1 (ternary-print n :b 9))
     (4 (ternary-print n :b 27))
     (t (format t "~a" (trytes-chars n))))
   nil)

;; Set a tape cell at a relative address

(defun set-addr (tape a v)
  (setf (aref (tape-vector tape)(mod (+ (tape-position tape) a)(tape-length tape)))v))

(defun run-io-engine (tape)
  (let* ((width (tape-width tape))
	 (length (tape-length tape))
	 (op (decode-op (tape-special tape)))
	 (a1)(a2)(jmp)
	 (dir (elt op 0))
	 (halt (elt op 4)))
    (unless halt
      (progn
	(setf jmp (mod (+ (tape-position tape) (* 3 dir)) length)
	      (tape-position tape) jmp
	      a1 (eval-addr tape (- dir))
	      a2 (eval-addr tape dir))
	(multiple-value-bind (new-a1 new-a2 sign)
	    (process-op (eval-addr tape a1)(eval-addr tape a2) op)
	  (progn
	    (set-addr tape a1 (coerce-width new-a1 :width width))
	    (set-addr tape a2 (coerce-width new-a2 :width width))
	    (setf (tape-halted tape) nil
		  (tape-special tape) nil
		  (tape-position tape)
		  (mod (+ (tape-position tape)
			  (eval-addr tape (+ (eval-addr tape 0) sign)))
		       length))))))))

