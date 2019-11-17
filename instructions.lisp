(in-package :cl-nutes)

;; Some common composed instructions 

;; Note that this subtraction can be used with all possible combinations of sign-based branches, including the Subleq known to be Turing-complete

; jmp (-(b=b-a))

(defun prg-sub (prg a b &key label jlabel (jmp (jmp+3 prg)))
  (prg-- prg a 'z1 :label label)
  (prg-- prg 'z1 'z2)
  (prg-- prg 'z1 'z1)
  (prg-- prg b 'z2)
  (prg-- prg 'z2 'z2 :jmp jmp :jlabel jlabel))

; jmp (-(b=b+a))

(defun prg-add (prg a b &key label jlabel (jmp (jmp+3 prg)))
  (prg-- prg a 'z1 :label label)
  (prg-- prg b 'z1)
  (prg-- prg 'z1 'z1 :jmp jmp :jlabel jlabel))

; jmp (-a)

(defun prg-jmp (prg a &key label jlabel (jmp (jmp+3 prg)))
  (prg-- prg a 'z1 :label label)
  (prg-- prg 'z1 'z1 :jmp jmp :jlabel jlabel))

; jmp(-(b=a))

(defun prg-mov (prg a b &key label jlabel (jmp (jmp+3 prg)))
  (prg-- prg a 'z1 :label label)
  (prg-- prg b b)
  (prg-- prg 'z1 b)
  (prg-- prg 'z1 'z1 :jmp jmp :jlabel jlabel))

; swap(a,b)

(defun prg-swap (prg a b &key label)
  (prg-- prg a 'z1 :label label)
  (prg-- prg b 'z2)
  (prg-- prg b b)
  (prg-- prg b 'z1)
  (prg-- prg a a)
  (prg-- prg a 'z2)
  (prg-- prg 'z1 'z1)
  (prg-- prg 'z2 'z2))

; jmp(-(a=a*2))

(defun prg-2a (prg a &key label jlabel (jmp (jmp+3 prg)))
  (prg-- prg a 'z1 :label label)
  (prg-- prg a 'z1)
  (prg-- prg 'z1 'z1 :jmp jmp :jlabel jlabel))

; jmp(-(a=a*3))

(defun prg-3a (prg a &key label jlabel (jmp (jmp+3 prg)))
  (prg-- prg a 'z1 :label label)
  (prg-- prg 'z1 'z2)
  (prg-- prg 'z1 'z2)
  (prg-- prg a 'z1)
  (prg-- prg 'z2 'z2)
  (prg-- prg 'z1 'z1 :jmp jmp :jlabel jlabel))

; jmp(b-a)

(defun prg-cmp (prg a b &key label (jlabel nil) (jmp (jmp+3 prg)))
  (prg-- prg a 'z1 :label label)
  (prg-- prg b 'z2)
  (prg-- prg 'z1 'z2)
  (prg-- prg 'z2 'z2)
  (prg-- prg 'z1 'z1 :jmp jmp :jlabel jlabel))

