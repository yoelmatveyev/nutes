;;; Some practical examples to add to your program

;; Example of usage:

; (defparameter prg1 (create-prg))
; (hello prg1)
; (defparameter tape1 (create-tape (expt 3 6)))(load-tape tape1 prg1)
; (run-tape tape1)				       


; Slow multiplication by repeating addition, A must be positive
; b=a*b

 (defun add-smul (prg a b &key label (jmp (jmp+3 prg)))
   (let ((loop (gensym))(next (gensym))(end (gensym)))
       (add-var prg 'v=1 1)
       (add-set prg b 'z3 :label label)
       (add-set prg a 'z4)
       (add-jsub prg 'v=1 'z4 :label loop :jmp (list end end next))
       (add-jadd prg 'z3 b :label next)
       (add-one-inst prg 'z1 'z1 :jmp loop)
       (add-one-inst prg 'z3 'z3 :label end :jmp jmp)))

; Find the nth pair of consequent Fibonacci numbers
; One of the most natural and efficient operations in our system

(defun add-fib-pair (prg n res1 res2 &key label (jmp (jmp+3 prg)))
  (let ((loop (gensym))(next (gensym)))
    (add-var prg 'v=1 1)
    (add-set prg 'v=1 res1 :label label)
    (add-one-inst prg res1 'z3 :label loop)
    (add-one-inst prg res2 'z3)
    (add-jsub prg 'v=1 n :jmp (list next loop loop))
    (add-one-inst prg 'z3 'z3 :label next :jmp jmp)))

;; Factorial
  
(defun add-factorial (prg n res &key label (jmp (jmp+3 prg)))
  (let ((loop (gensym))(end (gensym))(next (gensym)))
  (add-var prg 'v=1 1)
  (add-set prg n res :label label) 
  (add-jmp prg n :jmp (list end end loop))
  (add-jsub prg 'v=1 n :label loop :jmp (list end end next))
  (add-smul prg n res :label next :jmp loop)
  (add-jmp prg 'z1 :label end :jmp jmp)))

;; Basic input (up to 6 chars) and "Hello World"

(defun hello (prg)
  (add-var prg 'j1 8)
  (add-var prg 'hello (char-36 "Hello"))
  (add-var prg 'world (char-36 "World,"))
  (add-var prg 'name)
  (add-var prg 'input (char-36 "Name?"))
  (add-io prg 'j1 'input 'input (op-gen (list (+ (expt 3 8)(expt 3 7)) 1 0 0)))
  (add-io prg 'j1 'name 'name (op-gen (list (+ (expt 3 8) (expt 3 7)) 2 0 0)))
  (add-io prg 'j1 'hello 'world (op-gen (list (expt 3 8) 1 0 0)))
  (add-io prg 'j1 'name 'name (op-gen (list (+ (expt 3 8)(expt 3 7)) 1 0 0))))
			
					
