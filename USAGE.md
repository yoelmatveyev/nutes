In this example we'll work inside the package **cl-nutes**:

(asdf:operate 'asdf:load-op 'cl-nutes)

Let's create a tape of 3^12=531441. The default word width will be 36 trits.

CL-NUTES> (defparameter tape1 (create-tape (expt 3 12)))

Let's create a program for the assembler to work with:

CL-NUTES> (defparameter program1 (create-prg))

Let's compile and add the "Hello World" to the program:

CL-NUTES> (hello program1)

Let's load our program to the tape:

CL-NUTES> (load-tape tape1 program1)

Let's run it:

CL-NUTES> (run-tape tape1)

Name? Yoel

Hello World,Yoel

NIL

CL-NUTES> 
