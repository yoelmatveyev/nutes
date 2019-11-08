;; A couple of simple functions to represent numbers in balanced 3, 9 and 27 base.
;; Requires Unicode, because negative digits are overlined (perfectly works in Emacs)  

;; Balanced version of modulo

(defun bmod (n b)
	   (let ((a (floor b 2)))
	     (- (mod (+ n a) b) a)))

;; Getting the sequence of digits in 3^n balanced base

(defun 3n-split (n &optional (b 3))
	   (let ((bmod (bmod n b)))
	    (if (zerop n) (list 0)
		(cons bmod (3n-split (floor (- n bmod) b) b )))))

;; "Little-endian" version of the same

(defun 3n-digits (n &key (b 3) (l nil))
  (let* ((res (reverse (3n-split n b)))
	 (ln (length res)))
    (if l
	(if (<= l ln)
	    (setf res (subseq res (- ln l) ln))
	    (setf res (append (make-list (- l ln) :initial-element 0) res)))
	(if (= 0 (car res)) (pop res)))
    res))

 (defun ternary-list 
	     (n &optional (b 3))
	   (let
	       ((b3 #("1̅" "0" "1"))
		(b9 #("4̅" "3̅" "2̅" "1̅" "0" "1" "2" "3" "4"))
		(b27 #("D̅" "C̅" "B̅" "A̅" "9̅" "8̅" "7̅" "6̅" "5̅" "4̅" "3̅" "2̅" "1̅" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" ))
		(digits nil))
	     (setf digits (3n-digits n :b b))
	     (case b
	       (3 (mapcar (lambda (x) (elt b3 (+ x 1))) digits))
	       (9 (mapcar (lambda (x) (elt b9 (+ x 4))) digits))
	       (27 (mapcar (lambda (x) (elt b27 (+ x 13))) digits)))))

;; Produce a string representing a balanced 3,9 or 27-base number

(defun ternary-string (n &key (b 27)(l nil))
  (let* ((lst (ternary-list n b))
	 (ln (length lst)))
    (if l
	(if (< l ln)
	    (setf lst (subseq lst (- ln l) ln))
	    (setf lst (append (make-list (- l ln) :initial-element "0") lst))))
    (apply #'concatenate 'string lst)))

;; Print a number in 3,9 or 27 balanced base

(defun ternary-print (n &key (b 27) (l nil))
  (format t "~a" (ternary-string n :b b :l l)))
