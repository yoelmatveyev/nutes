(defpackage :cl-nutes 
  (:use :common-lisp)
  (:export width
	   range
	   power
	   tape-halted
	   tape-position
	   tape-counter
	   tape-length
	   tape-special
	   create-tape
	   one-step
	   run-tape
	   bmod
	   3n-digits
	   ternary-print
	   op-split
	   op-gen
	   36-char
	   char-36
	   decode-op
	   convert-minus
	   prg-label-value
	   prg-list-labels
	   create-prg
	   inst-table
	   add-code-prg
	   add-var
	   set-var
	   add-3jmp
	   add-one-inst
	   add-jsub
	   add-jadd
	   add-swap
	   add-set
	   add-jmp
	   add-2a
	   add-3a
	   add-cmp
	   add-smul
	   hello
	   add-factorial
	   add-io
	   add-fib-pair
	   ))

(in-package :cl-nutes)
