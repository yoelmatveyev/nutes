;; Some practical examples to add to your program

; Slow multiplication by repeating addition, a must be positive
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
  (add-smul prg n res :label loop :jmp next)
  (add-jsub prg 'v=1 n :label next :jmp (list end end loop))
  (add-jmp prg 'z1 :label end :jmp jmp)))
