(in-package :cl-nutes)

;;; Some practical examples of Nutes programs

;; Example of usage:

; (defparameter prg1 (create-prg))
; (hello prg1)
; (defparameter tape1 (create-tape (expt 3 6)))(load-tape tape1 prg1)
; (run-tape tape1)				       


; Slow multiplication by repeating addition, A must be positive
; b=a*b

 (defun prg-smul (prg a b &key label (jmp 'jmp+3))
   (let ((loop (gensym))(next (gensym))(end (gensym)))
       (prg-mov prg b 'z3 :label label)
       (prg-mov prg a 'z4)
       (prg-sub prg 'v=1 'z4 :label loop :jmp (list next end end))
       (prg-add prg 'z3 b :label next)
       (prg-- prg 'z1 'z1 :jmp loop)
       (prg-- prg 'z3 'z3 :label end :jmp jmp)))

; Find the nth pair of consequent Fibonacci numbers
; One of the most natural and efficient operations in our system

(defun prg-fib-pair (prg n res1 res2 &key label (jmp 'jmp+3))
  (let ((loop (gensym))(next (gensym)))
    (prg-- prg 'v=-1 res1 :label label)
    (prg-- prg res1 'z3 :label loop)
    (prg-- prg res2 'z3)
    (prg-add prg 'v=-1 n :jmp (list loop loop next))
    (prg-- prg 'z3 'z3 :label next :jmp jmp)))

;; Factorial
  
(defun prg-factorial (prg n res &key label (jmp 'jmp+3))
  (let ((loop (gensym))(end (gensym))(next (gensym)))
  (prg-mov prg n res :label label) 
  (prg-sign prg n :jmp (list loop end end))
  (prg-add prg 'v=-1 n :label loop :jmp (list next end end))
  (prg-smul prg n res :label next :jmp loop)
  (prg-sign prg 'z1 :label end :jmp jmp)))

;; Basic input and "Hello World", requires width>=36 

(defun hello (prg)
  (prg-var prg 'hello (chars-trytes "Hello "))
  (prg-var prg 'world (chars-trytes "World,"))
  (prg-var prg 'name (chars-trytes "Name? "))
  (prg-var prg 'io1 9)
  (prg-io prg 'io1 'name 0)
  (prg-io prg 'v=1 'hello 0) 
  (prg-io prg 'v=1 'world 0)
  (prg-io prg 'v=1 'name 0))

;; Determine the highest trit of the first operand, left-shift both operands and add the resulting trit to the second operand. This is a basic construction block for right shifts, tritwise operations and double-operand arithmetics.

(defun prg-movtrit (prg l h &key (label (gensym)) (jmp 'jmp+3))
  (let ((j- (gensym))
	(j0 (gensym))
	(j+ (gensym)))	
  (prg-- prg l 'z1 :label label)
  (prg-- prg 'z1 'z2)
  (prg-- prg 'z1 'z2)
  (prg-- prg l 'z1 :jmp (list j- (jmp-r prg 3) j+))
  (prg-- prg 'z1 'z2 :jmp (list j- j0 j+))
  (prg-- prg 'v=-1 'z3 :label j- :jmp j0)
  (prg-- prg 'v=1 'z3 :label j+)
  (prg-- prg 'z1 'z1 :label j0)
  (prg-- prg 'z2 'z2)
  (prg-mul3 prg h)
  (prg-- prg h 'z3)
  (prg-- prg 'z3 'z3 :jmp jmp)))

;; Add 2 operands, shift the third operand to the left and add the overflow sign to it. 

  (defun prg-overadd (prg a b h &key (label (gensym)) (jmp 'jmp+3))
     (let ((j- (gensym))
	   (j0 (gensym))
	   (j+ (gensym)))
  (prg-- prg a 'z1 :label label)
  (prg-- prg 'z1 'z2)
  (prg-- prg b 'z1 :jmp (list j0 (jmp-r prg 3) j0))
  (prg-- prg 'z1 'z2 :jmp (list j- j0 j+))
  (prg-- prg 'v=-1 'z3 :label j- :jmp j0)
  (prg-- prg 'v=1 'z3 :label j+ :jmp j0)
  (prg-- prg 'z1 'z1 :label j0)
  (prg-- prg 'z2 'z2)
  (prg-mul3 prg h)
  (prg-- prg h 'z3)
  (prg-- prg 'z3 'z3 :jmp jmp)))

(defun prg-shift (prg a n &key (label (gensym)) (jmp 'jmp+3))
 (let ((loop (gensym))
       (out (gensym))
       (movtrit (gensym)))
  (prg-var prg 'v=36 -36)
  (prg-- prg 'v=36 'z4 :label label)
  (prg-- prg n 'z4)
  (prg-- prg 'v=1 'z1)
  (prg-- prg 'z1 'z4)
  (prg-- prg 'z1 'z1)
  (prg-var prg 'v=-1 -1)
  (prg-- prg 'v=-1 'z1 :label loop)
  (prg-- prg 'z1 'z4)
  (prg-- prg 'z1 'z1 :jmp (list movtrit out out))
  (prg-movtrit prg a 'z5 :label movtrit :jmp loop)
  (prg-- prg 'z4 'z4 :label out)
  (prg-- prg a a)
  (prg-- prg 'z5 'z1)
  (prg-- prg 'z1 a)
  (prg-- prg 'z5 'z5)
  (prg-- prg 'z1 'z1 :jmp jmp)))

;; Define the tape's width programmatically. Normally it's predefined.

(defun prg-width (prg)
  (let ((loop (gensym))(end (gensym)))
    (prg-- prg 'width 'width) 
    (prg-- prg 'v=1 'z3)
    (prg-add prg 'v=1 'width :label loop)
    (prg-mul3 prg 'z3 :jmp (list end end loop))
    (prg-- prg 'z3 'z3 :label end)))

;; Invert the first operand rigth to left and move the result to the second operand

(defun prg-invert (prg a b &key (label (gensym)) (jmp 'jmp+3))
 (let ((loop (gensym))
       (end (gensym))
       (next (gensym))
       (j- (gensym))
       (j0 (gensym))
       (j+ (gensym)))
   (prg-- prg 'width 'z4 :label label)
   (prg-- prg 'v=1 'z5)
   (prg-- prg 'v=-1 'z6)
   (prg-- prg a 'z1 :label loop)
   (prg-- prg 'z1 'z2)
   (prg-- prg 'z1 'z2)
   (prg-- prg a 'z1 :jmp (list j- (jmp-r prg 3) j+))
   (prg-- prg 'z1 'z2 :jmp (list j- j0 j+))
   (prg-- prg 'z5 'z3 :label j-)
   (prg-- prg b 'z3)
   (prg-- prg 'z3 'z3 :jmp j0)
   (prg-- prg 'z6 'z3 :label j+)
   (prg-- prg b 'z3)
   (prg-- prg 'z3 'z3)
   (prg-- prg 'z1 'z1 :label j0)
   (prg-- prg 'z2 'z2)
   (prg-add prg 'v=1 'z4 :jmp (list end end next))
   (prg-mul3 prg 'z5 :jmp next)
   (prg-- prg 'z6 'z6)
   (prg-- prg 'z5 'z6 :jmp loop)
   (prg-- prg 'z1  'z1 :label end :jmp jmp)))

(defun prg-trit-count (prg a b &key label (jmp 'jmp+3))
  (let ((out (gensym))
	(j- (gensym))
	(j0 (gensym))
	(j+ (gensym))
	(j3 (gensym)))
    (prg-- prg a 'z1 :label label :jmp (list j3 out j3))
    (prg-- prg 'z1 'z2 :jlabel j3)
    (prg-- prg 'z1 'z2)
    (prg-- prg a 'z1 :jmp (list j- j3 j+))
    (prg-- prg 'z1 'z2 :jmp (list j- j0 j+))
    (prg-- prg 'v=-1 'z3 :label j- :jmp j0)
    (prg-- prg 'v=1 'z3 :label j+)
    (prg-- prg 'z1 'z1 :label j0)
    (prg-- prg 'z2 'z2)
    (prg-- prg b 'z3)
    (prg-- prg 'z3 'z3 :jmp label)
    (prg-- prg 'z3 'z3 :label out :jmp jmp)))
  
