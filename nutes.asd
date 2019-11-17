(defsystem :nutes
  :name "Nutes"
  :version "0.0.6"
  :maintainer "Yoel Matveyev"
  :author "Yoel Matveyev"
  :licence "GNU General Public License v3.0"
  :description "OISC ternary virtual machine"
  :long-description "A signwise symmetrical balanced ternary base virtual machine with only one instruction"
  :components ((:file "package")
	       (:file "machine")
	       (:file "ternary-print")
       	       (:file "opcodes")
	       (:file "io")
	       (:file "asm")
	       (:file "instructions")	
	       (:file "examples")))
