;; A couple of simple functions to represent number in balanced 3, 9 and 27 base.


;; Balanced version of modulo

 (defun bmod (n b)
	   (let ((a (floor b 2)))
	     (- (mod (+ n a) b) a)))

;; Getting the sequence of digits in 3^n balanced base

(defun 3n-digits (n &optional (b 3))
	   (let ((bmod (bmod n b)))
	    (if (zerop n) (list 0)
		(cons bmod (3n-digits (floor (- n bmod) b) b )))))

;; "Little-endian" version of the same

(defun 3n-digits2 (n &optional (b 3))
	   (let ((res (reverse (3n-digits n b))))
	     (if (= 0 (car res)) (pop res))
	     res))

 (defun ternary-list 
	     (n &optional (base 3))
	   (let
	       ((b3 #("1̅" "0" "1"))
		(b9 #("4̅" "3̅" "2̅" "1̅" "0" "1" "2" "3" "4"))
		(b27 #("D̅" "C̅" "B̅" "A̅" "9̅" "8̅" "7̅" "6̅" "5̅" "4̅" "3̅" "2̅" "1̅" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" ))
		(digits nil))
	     (setf digits (3n-digits2 n base))
	     (case base
	       (3 (mapcar (lambda (x) (elt b3 (+ x 1))) digits))
	       (9 (mapcar (lambda (x) (elt b9 (+ x 4))) digits))
	       (27 (mapcar (lambda (x) (elt b27 (+ x 13))) digits)))))

;; Produce a string representing a balanced 3,9 or 27-base number

(defun ternary-string (n &optional (base 3))
	   (apply #'concatenate 'string (ternary-list n base)))

;; Print a number in 3,9 or 27 balanced base

(defun ternary-format (n &optional (base 3))
	   (format t (apply #'concatenate 'string (ternary-list n base))))
