(load "nutes.lisp")

;;; This test demonstrates Turing-completeness of our OISC virtual machine by implementing a generalized version of SUBLEQ known to be Turing-complete. The implementation allows to set different or identical branch addresses for positive, negative and zero result.

;; generalized subleq generator, counting operands and jumps from the entry point (-3 15 a)

(defun subleq (a b j+ j0 j-)
  (list 0 0 -3 15 a -6 12 -5 -9 9 -9
	(- b 9)
	6 -11 -14 6 -14 3 3 3
	(- j+ 12)
	(- j0 12)
	(- j- 12)))

;; setting the test

(defun test-subleq (tape a b x y)
  (let ((v (tape-vector tape))
	(p (tape-position tape))
	(h (tape-halted tape))
	(i (tape-initial tape)))
  (loop for n from 0 to 728 do
       (setf (elt v n) 0)))
  (setf p 0)
  (setf h nil)
  (setf i 0)
    (program-tape tape
		  (subleq a b 100 200 300) -3)
    (program-tape tape (list x) a)
    (program-tape tape (list y) b)
    (run-tape tape)
  tape)

;; running a test

(initiate-tape tape1 729 36)

(test-subleq tape1 120 140 12 150)
