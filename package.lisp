(defpackage :cl-nutes 
  (:use :common-lisp)
  (:export st-width
	   st-range
	   st-power
	   width
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
	   3n-split
	   3n-digits
	   ternary-string
	   ternary-print
	   op-split
	   op-gen
	   trytes-chars
	   chars-trytes
	   eval-addr
	   decode-op
	   convert-minus
	   ternary-input
	   ternary-output
	   set-addr
	   prg-label-value
	   prg-list-labels
	   create-prg
	   prg-var
	   prg-3jmp
	   prg--
	   prg-sub
	   prg-add
	   prg-swap
	   prg-mov
	   prg-sign
	   prg-mul2
	   prg-mul3
	   prg-cmp
	   prg-smul
	   hello
	   prg-factorial
	   prg-io
	   prg-fib-pair
	   prg-movtrit
	   prg-overadd
	   prg-shift
	   prg-width
	   prg-invert
	   prg-trit-count
	   ))

(in-package :cl-nutes)
	
