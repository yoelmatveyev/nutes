(load "lisp/nutes/assembler.lisp")

;; Process opcodes

(defun process-op (a1 a2 de-op)
  (let* ( ;(dir (elt de-op 0))
	 (flags (elt de-op 1))
	 (args (elt flags 0))
	 (op (elt de-op 2))
	 (m (op-gen (list (elt flags 6)(elt flags 7)) :b 3)))
  (case op
    (1
     (case args
       (0 (ternary-output a1 :mode m)
	  (ternary-output a2 :mode m))
       (-1 (ternary-output a1 :mode m))
       (1 (ternary-output a2 :mode m))))
    (2
      (case args
	(0 (setf a1 (ternary-input :mode m))
	   (setf a2 (ternary-input :mode m)))
	(-1 (setf a1 (ternary-input :mode m)))
	(1 (setf a2 (ternary-input :mode m))))))   
  (values a1 a2)))
