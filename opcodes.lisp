(in-package :cl-nutes)

;; Process opcodes

(defun process-op (a1 a2 de-op)
  (let ((param (elt de-op 2))
	(op (elt de-op 3))
	(sign 0))    
  (case op
    (1 (ternary-output a1 :mode param))
    (0 (ternary-output a1 :mode param)
       (setf a1 (ternary-input :mode param)))
    (-1 (setf a1 (ternary-input :mode param))))  
  (values a1 a2 sign)))
