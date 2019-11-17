(in-package :cl-nutes)

;;; Some practical examples to add to your program

;; Example of usage:

; (defparameter prg1 (create-prg))
; (hello prg1)
; (defparameter tape1 (create-tape (expt 3 6)))(load-tape tape1 prg1)
; (run-tape tape1)				       


; Slow multiplication by repeating addition, A must be positive
; b=a*b

 (defun prg-smul (prg a b &key label (jmp (jmp+3 prg)))
   (let ((loop (gensym))(next (gensym))(end (gensym)))
       (prg-var prg 'v=1 1)
       (prg-mov prg b 'z3 :label label)
       (prg-mov prg a 'z4)
       (prg-sub prg 'v=1 'z4 :label loop :jmp (list next end end))
       (prg-add prg 'z3 b :label next)
       (prg-- prg 'z1 'z1 :jmp loop)
       (prg-- prg 'z3 'z3 :label end :jmp jmp)))

; Find the nth pair of consequent Fibonacci numbers
; One of the most natural and efficient operations in our system

(defun prg-fib-pair (prg n res1 res2 &key label (jmp (jmp+3 prg)))
  (let ((loop (gensym))(next (gensym)))
    (prg-var prg 'v=-1 -1)
    (prg-- prg 'v=-1 res1 :label label)
    (prg-- prg res1 'z3 :label loop)
    (prg-- prg res2 'z3)
    (prg-add prg 'v=-1 n :jmp (list loop loop next))
    (prg-- prg 'z3 'z3 :label next :jmp jmp)))

;; Factorial
  
(defun prg-factorial (prg n res &key label (jmp (jmp+3 prg)))
  (let ((loop (gensym))(end (gensym))(next (gensym)))
  (prg-var prg 'v=-1 -1)
  (prg-mov prg n res :label label) 
  (prg-jmp prg n :jmp (list loop end end))
  (prg-add prg 'v=-1 n :label loop :jmp (list next end end))
  (prg-smul prg n res :label next :jmp loop)
  (prg-jmp prg 'z1 :label end :jmp jmp)))

;; Basic input (up to 6 chars) and "Hello World"

(defun hello (prg)
  (prg-var prg 'j1 8)
  (prg-var prg 'hello (char-36 "Hello"))
  (prg-var prg 'world (char-36 "World,"))
  (prg-var prg 'name)
  (prg-var prg 'input (char-36 "Name?"))
  (prg-io prg 'j1 'input 'input (op-gen (list (+ (expt 3 8)(expt 3 7)) 1 0 0)))
  (prg-io prg 'j1 'name 'name (op-gen (list (+ (expt 3 8) (expt 3 7)) 2 0 0)))
  (prg-io prg 'j1 'hello 'world (op-gen (list (expt 3 8) 1 0 0)))
  (prg-io prg 'j1 'name 'name (op-gen (list (+ (expt 3 8)(expt 3 7)) 1 0 0))))
			
					
