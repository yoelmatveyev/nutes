;; Structure for a rudimental assembler emulating a conventional computer

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

;; Checking whether an operand affects a jump or an instruction

(defun modifp (addr op)
  (if
   (< (abs (- addr op)) 2)
   t))

(defun find-addr (prg x)
  (let ((addr x))
    (if (symbolp x)
	(setf addr
	      (cadr (gethash x (program-labels prg))))
	x)))      

;; Generate a single instruction with an emulated absolute address
;; Perform basic checks for halting and self-modification

(defun one-inst (addr a b j)
  (let ((lst))
    (setf a (- a addr))
    (setf b (- b addr))
    (setf j (- j addr))
    (setf lst (list a j b))
  (values
   lst
   (remove nil
	   (list
	    (if
	     (and
	      (= b a)
	      (zerop j))
	     'halt)
	    (if
	     (or
	      (modifp addr a)
	      (modifp addr b))
	     'op-modif)
	    (if
	     (or
	      (= j a)
	      (= j b))
	     'jmp-addr-modif)
	    (if
	     (or
	      (modifp j a)
	      (modifp j b))
	     'jmps-modif)
	    (if (= a b)
		'zero))))))

;; Add an emulated absolute jump for an instruction to the data section of a program

(defun add-3jmp (prg addr jmp &key (label nil) (ignore nil))
  (let ((jmpaddr
	 (- (program-first prg) 2)))
    (setf addr (find-addr prg addr))
    (if (and
	 (not ignore)
	 (not (gethash label (program-labels prg))))
	(progn (if label
	(setf (gethash label (program-labels prg))
	      (list
	       'jmp
	       jmpaddr
	       addr)))
    (if (not (listp jmp))
	(setf jmp (make-list 3 :initial-element (find-addr prg jmp))))
    (setf (program-data prg)
	      (reduce #'cons
		(mapcar (lambda (_) (- (find-addr prg _) addr)) jmp)
                     :initial-value (program-data prg)
                     :from-end t))
  (setf (program-length prg)
	(+ (program-length prg) 3))
  (setf (program-first prg)
	(- (program-first prg) 3))))
    jmpaddr))

;; Add a variable

(defun add-var (prg label &optional (value 0))
  (if (not (gethash label (program-labels prg)))
      (progn
	(setf (program-data prg) (append (list value) (program-data prg)))
	(setf (program-first prg) (1- (program-first prg)))
	(setf (program-length prg) (1+ (program-length prg)))
	(setf (gethash label (program-labels prg))
	      (list 'var (program-first prg))))))

;; Set an existing variable

(defun set-var (prg label value)
  (setf (nth (-
	      (cadr (gethash label (program-labels prg)))
	      (program-first prg))
	     (program-data prg))
	value))

;; Check if a jump label exists, mark it as pending if not
;; Temporarity set the pointer to it to the next instruction

(defun chk-jmp (prg jmp addr)
  (let
      ((labels (program-labels prg))
       (next (+ 5 (program-last prg))))
    (if (or (symbolp jmp)(numberp jmp))
	(setf jmp (make-list 3 :initial-element jmp)))
    (loop for x from 0 to 2 do
	 (let ((j (nth x jmp)))
           (if (symbolp j)
	       (cond ((not (gethash j labels))
		      (setf (gethash j labels)
			    (list '? (make-hash-table)))
		      (let ((h (cadr (gethash j labels))))
			(setf (gethash addr h)
			      (make-list 3 :initial-element '+))
			(setf (nth x (gethash addr h)) '?)
			)
		      (setf (nth x jmp) next))
		     ((eql (car (gethash j labels)) '?)
		      (let ((h (cadr (gethash j labels))))
			(if (not (gethash addr h))
			    (setf (gethash addr h)
				  (make-list 3 :initial-element '+)))
			(setf (nth x (gethash addr h)) '?)
			)
		      (setf (nth x jmp) next))))))
    jmp))

;; When a label appears, set all it previous occurences to the proper address  

(defun chk-label (prg label addr newjmp)
  (let ((length (length (program-data prg))))
    (if (eql (car (gethash label (program-labels prg))) '?)
	(let
	    ((jaddr)
	     (ad-ht (cadr (gethash label (program-labels prg)))))
	  (loop for a being each hash-key of ad-ht using (hash-value v) do
		(loop for n from 0 to 2 do
		     (setf jaddr (nth (1+ a) (program-code prg)))
		     (if (eql (nth n v) '?)
			 (setf (nth (+ length jaddr n a)
				    (program-data prg))
			       (- addr a)))))))
   (setf (gethash label (program-labels prg))
	  (list 'inst addr newjmp))))

;; Determine the default jump address for the next instruction

(defun jmp+3 (prg)
  (cadr (gethash 'jmp+3 (program-labels prg))))

;; Add one instruction to a program, add a 3-jump to the data if needed
;; Jump to the next instruction by default

(defun add-one-inst
    (prg a b &key
	       (jmp (jmp+3 prg))
	       (forbid '('op-modif 'jmp-addr-modif))
	       label jlabel)
  (let* ((addr (+ (program-last prg) 2))
	 (newjmp)
	 (abs-jump nil)
	 (nj3 nil)
	 (jlabel-existed nil)
	 (jmp+3 (jmp+3 prg)))
    (if (and (symbolp a) (not (gethash a (program-labels prg))))
	(add-var prg a))
    (setf a (find-addr prg a))
    (if (and (symbolp b) (not (gethash b (program-labels prg))))
	(add-var prg b))
    (setf b (find-addr prg b))
    (if (gethash jlabel (program-labels prg)) (setf jlabel-existed t))
  (if (not (eql jmp jmp+3))
      (if jlabel-existed
	  (setf newjmp (caddr (gethash jlabel (program-labels prg))))
	  (setf newjmp (- (program-first prg) 2)))
      (progn (setf newjmp jmp+3)(setf nj3 t)))
  (multiple-value-bind (inst flags)
      (one-inst addr a b newjmp)
      (if (not (intersection forbid flags))
	(progn
	  (setf (program-code prg) (append (program-code prg) '(0 0 0)))
	  (if label (chk-label prg label addr newjmp))
	  (setf jmp (chk-jmp prg jmp addr))
          (if (or (symbolp jmp)(numberp jmp))
	      (setf jmp (make-list 3 :initial-element (find-addr prg jmp)))
	      (setf jmp (mapcar (lambda (_) (find-addr prg _)) jmp)))
	  (setf jmp
		(- (if jlabel
		       (if
			(setf abs-jump (add-3jmp prg addr jmp :label jlabel :ignore nj3))
			abs-jump
			(caddr (gethash jlabel (program-labels prg))))
	               (add-3jmp prg addr jmp :ignore nj3))
      		   addr))
	  (loop for n from 0 to 2 do
	       (let ((length (length (program-code prg))))
		 (setf (nth (+ length n -3) (program-code prg)) (nth n inst)))) 
	  (setf (program-last prg)(1+ addr))
	  (setf (program-length prg)
		(- (program-last prg)(program-first prg) -1)))))))

;; Create anonymous programs with the common +3 unconditional jump preset 

(defun make-prg ()
  (let ((prg
	 (make-program :direction 1
		       :first -1
		       :last -2
		       :entry 0
		       :length 0 
		       :labels (make-hash-table))))

; 3 default placeholder variables for composing basic instructions
; Should alway be reset to zero after using them
    
    (mapcar (lambda (_) (add-var prg _)) (list 'z1 'z2 'z3))

; Default jump to the next instruction to the right
    
    (add-3jmp prg 0 3 :label 'jmp+3)
prg))