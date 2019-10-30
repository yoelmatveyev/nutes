;; Some common composed instructions 

;; Note that this subtraction can be used with all possible combinations of sign-based branches, including the Subleq known to be Turing-complete

; jmp (sign (b=b-a))

(defun add-jsub (prg a b &key label (jlabel nil) (jmp (jmp+3 prg)))
  (add-one-inst prg a 'z1 :label label)
  (add-one-inst prg 'z1 'z2)
  (add-one-inst prg 'z1 'z1)
  (add-one-inst prg b 'z2)
  (add-one-inst prg 'z1 'z2); These two instructions could be removed,
  (add-one-inst prg 'z2 'z2); but the jump would then be according to the sign of -(b-a)
  (add-one-inst prg 'z1 'z1 :jmp jmp :jlabel jlabel))

; jmp (sign (b=b+a))

(defun add-jadd (prg a b &key label (jlabel nil) (jmp (jmp+3 prg)))
  (add-one-inst prg a 'z1 :label label)
  (add-one-inst prg b 'z1)
  (add-one-inst prg 'z1 'z2)
  (add-one-inst prg 'z1 'z1)
  (add-one-inst prg 'z2 'z2 :jmp jmp :jlabel jlabel))

; jmp (sign (a))

(defun add-jmp (prg a &key label (jlabel nil) (jmp (jmp+3 prg)))
  (add-one-inst prg a 'z1 :label label)
  (add-one-inst prg 'z1 'z2)
  (add-one-inst prg 'z1 'z1)
  (add-one-inst prg 'z2 'z2 :jmp jmp :jlabel jlabel))

; b=a

(defun add-set (prg a b &key label)
  (add-one-inst prg a 'z1 :label label)
  (add-one-inst prg b b)
  (add-one-inst prg 'z1 b)
  (add-one-inst prg 'z1 'z1))

; swap(a,b)

(defun add-swap (prg a b &key label)
  (add-one-inst prg a 'z1 :label label)
  (add-one-inst prg b 'z2)
  (add-one-inst prg b b)
  (add-one-inst prg b 'z1)
  (add-one-inst prg a a)
  (add-one-inst prg b b)
  (add-one-inst prg a 'z2)
  (add-one-inst prg 'z1 'z1)
  (add-one-inst prg 'z2 'z2))

; a=a*2

(defun add-2a (prg a &key label)
  (add-one-inst prg a 'z1 :label label)
  (add-one-inst prg a 'z1)
  (add-one-inst prg 'z1 'z1))

; a=a*3

(defun add-3a (prg a &key label)
  (add-one-inst prg a 'z1 :label label)
  (add-one-inst prg 'z1 'z2)
  (add-one-inst prg 'z1 'z2)
  (add-one-inst prg a 'z1)
  (add-one-inst prg 'z1 'z1)
  (add-one-inst prg 'z2 'z2))

; jmp (sign (b-a))

(defun add-cmp (prg a b &key label (jlabel nil) (jmp (jmp+3 prg)))
  (add-one-inst prg a 'z1 :label label)
  (add-one-inst prg b 'z2)
  (add-one-inst prg 'z3 b)
  (add-one-inst prg 'z1 'z2)
  (add-one-inst prg 'z2 'z2)
  (add-one-inst prg 'z1 'z1 :jmp jmp :jlabel jlabel))

