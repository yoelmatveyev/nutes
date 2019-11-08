;; Interrupt and IO engine

(defparameter op-pointer (expt 3 9))

(defparameter 3^6 (expt 3 6))

;; Decode and encode operands for an interrupt

(defun op-split (n)
      (coerce (3n-digits n :b op-pointer :l 4) 'vector))

(defun op-gen (x &key (b op-pointer))
  (unless b (setf b (expt 3 b))) (reduce
   (lambda (h l)(+ (* h b) l)) 
   (coerce x 'list)))

;; Decode and encode strings up to 6 characters

(defun 36-char (n)
      (let ((str)
	    (d (3n-split n 3^6)))
	(loop for x from 0 to 5 do
	     (if (nth x d)
		 (progn
		   (push (code-char (abs (nth x d))) str)
		   (if (< (nth x d) 0)(push (code-char #x0305) str)))))
      		 (coerce str 'string)))

(defun char-36 (str &key neg)
  (let ((codes)(res)(length (length str)))
    (if (>= length 6)
	(setf str (subseq str 0 6))
	(setf str
	      (concatenate 'string str (make-string (- 6 length) :initial-element #\Space)))) 
    (setf codes (map 'list (lambda (x) (char-code x)) str))
    (setf res (reduce
     (lambda (h l)(+ (* h 3^6) l))
     codes))(if neg (- res) res)))

;; Find actual addresses of the next jump, operands and opcode

(defun op-addr (tape spl)
  (let ((pos (tape-position tape))
	(l (tape-length tape)))
  (map 'vector
   (lambda (x)
     (elt (tape-vector tape) (mod (+ pos x) l)))
   (coerce spl 'vector))))

;; Decode opcodes

(defun decode-op (n)
  (let* ((split (op-split n))
	 (flags (3n-digits (elt split 0)))
	 (dir)(op)(arg1)(arg2)(halt))
    (if (car flags)
	(setf dir (car flags)))
    (if dir
	(progn
	  (setf flags
		(coerce (mapcar (lambda (_) (* _ dir))
			(cdr
			 (append flags
				 (make-list
				  (- 9 (length flags)) :initial-element 0))))
			'vector))
	  (setf op (* (elt split 1) dir))
	  (setf arg1 (* (elt split 2) dir))
	  (setf arg2 (* (elt split 3) dir))
	  (if (zerop op) (setf halt t)))
	(setf halt t))
    (coerce (list dir flags op arg1 arg2 halt) 'vector)))

;; Input
;; Negative digits are preceeded by "|"

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
    (op-gen (reverse (mapcar (lambda (x)(* x sign)) res)) :b b)))

;; Input (invalid digits unchecked)

(defun ternary-input (&key (mode 0))
  (let ((l (read-line)))
   (case (abs mode)
     (0 (char-36 (if (> (length l) 6) (subseq l 0 6) l)))
     (1 (convert-minus l :b 3)) 
     (2 (convert-minus l :b 9))
     (3 (convert-minus l :b 27))
     (4 (parse-integer l)))))

;; Output

 (defun ternary-output (n &key (mode 0))
   (case mode
     (-4 (print n))
     (-3 (ternary-print n :b 27))
     (-2 (ternary-print n :b 9))
     (-1 (ternary-print n :b 3))
     (0 (format t "~a" (36-char n)))
     (1 (ternary-print n :b 3 :l 36))
     (2 (ternary-print n :b 9 :l 18))
     (3 (ternary-print n :b 27 :l 12))
     (4 (print n)))
   nil)

(load "opcodes.lisp")

(defun run-io-engine (tape)
  (let* ((length (tape-length tape))
	 (special (tape-special tape))
	 (op-split (op-split special))
	 (op-addr (op-addr tape op-split))
	 (op-addr2)
	 (op-value1)
	 (op-value2)
	 (op (elt op-addr 3))
	 (jaddr)
	 (de-op (decode-op op))
	 (flags (elt de-op 1))
	 (args))
    (setf args
	  (if flags (elt flags 0) 0))
    (case args
      (-1 (setf (elt op-addr 2) (elt op-addr 1)))
      (1 (setf (elt op-addr 1) (elt op-addr 2))))
    (setf op-addr2 (op-addr tape op-addr))
    (setf op-value1 (elt op-addr2 1))
    (setf op-value2 (elt op-addr2 2))
    (setf jaddr (elt op-addr2 0))
    (setf op-addr (subseq op-addr 1 3))
    (unless (elt de-op 5)
	(progn
	  (multiple-value-bind (a1 a2) (process-op op-value1 op-value2 de-op)
	    (setf (elt (tape-vector tape)
		       (mod (+ (tape-position tape) (elt op-addr 0)) length))
		  a1)
            (setf (elt (tape-vector tape)
		       (mod (+ (tape-position tape) (elt op-addr 1)) length))
		  a2))
	    (setf (tape-halted tape) nil)
	    (setf (tape-special tape) nil)
	    (setf (tape-position tape)
		  (mod (+ (tape-position tape) jaddr) length))))))

;; Wrapper for the IO engine

(defun input-output (tape)
  (if (= width 36)
      (run-io-engine tape)
      (tape-special tape)))
