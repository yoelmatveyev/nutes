# Introduction

In 1959, a group of scientists at the Moscow State University developed the unique experimental computer Setun, which used the balanced ternary numeral system instead of binary. The design has been was proven to be remarkably stable, efficient and elegant. Although the production was disbanded in 1965, major computer scientists, including Donald Knuth, still find the ternary computers exceptionally attractive.

Nutes (reverse of Setun) is a simple abstract machine, a ternary variety of a OISC (One Instruction Set Computer). Invented as a tribute to early Soviet computers,  this machine, also implementing an esoteric programming language, exhibits a number of unusual features, reminiscent of the Turing machines and cellular automata.

The machine is Turing-complete, because it is easy to implement in it other instructions known to be sutiable for OISC designs by a simple sequence of operations, e.g. "subtract and branch if negative".

# Description

The Nutes machine operates on either an infinite (theoretically) or circular memory tape divided into cells. Each cell contains a word of 36 trits, the ternary equivalent of bits.

The head of the machine reads the current cell and two other cells next to it on both sides of the tape:

x j y

The cell j contains a relative pointer to three other cells, which contain a series of jump addresses relative to the current cell:

j- j0 j+

The cells x and y contain pointers to two other cells, v1 and v2, which contain the actual operands. The pointers are relative to the current position of the machine head.

The choice between the jumps depends on the sum of signs of v1 and v2. The head jumps to j- number of cells, if the result is negative, to j+ number of cells, if the result is positive, or to j0 cells, if the result is zero. 

The computation continues until both j0 and v1-v2 are 0, in which case the machine interrupts and passes the original value of the operands to the external interrupt and IO engine, which can only use adressed relative to the current position of the tape head. The value of the operand is split into 4 9-trits words, which point to four addresses, which contain the next jump address, two actual addresses of the IO angine operands and the opcode. The opcode is further split into 4 9-trit words: Flags, opcode and two additional parameters.

All operations of the external engine must preserve the sign/direction symmetry explained below. The tape halts, if the flags or the opcode is 0.

If the computation continues, the values of v1 and v2 are replaced, respectively, by v1-v2 and v2-v1.

# Examples:

## 1 

..1 1 -2 (Head)-> 4 -3 4 20 18 0

The head reads two operand pointers, both located by -2 cells to the left of its current location. The sign sum of the operands, both being 1, is positive (1+1). The indirect branching pointer directs to the branching sequence of 20 18 0. Since the jump address for the positive case is 0 and the result of subtraction is 0, the machine halts:

..0 0 -2 4 -3 4 20 (Halted) 18 0

# Assember and IO engine

A rudimentary interactive assembler indended to be used in REPL and a basic set of composed instructions, such as addition, subtraction, left shift (identical in balanced ternary to multiplication by 3), setting variables, comparison, swapping etc. allows to write simple programs. Two opcodes with two flags are currently defined for the IO engine, enabling alphanumeric, ternary, base-9. base-27 and decimal input and output for 1 or 2 operands.

Examples include slow multiplication by repeated addition, finding consequent pairs of Fibonacci numbers (one of the most natural operations in our virtual machine), factorial and basic input (up to 6 characters) combined with "Hello World".

# Rationale

While the double negation and triple branching based on the signs of the operands may seem somewhat overcomplicated, this instruction exhibit an unusual symmetry, impossible in conventional instruction sets. Because the cells do not have absolute addresses, the machine is truly Turing-complete, assuming that the tape is infinite. There is no essencial bias in favor of positive and negative numbers, as well as between the right and left direction of movement on the tape, although the sign and the direction are inherently linked to each other. Any program rewritten completely in reverse with the sign of all its cells reversed as well, performs exactly the same computation (except that all results, naturally, also have their sign reversed). Using the same example as above negated in reverse:

0 -18 -20 -4 3 (Head)-> -4 2 -1 -1

The sign sum is now negative. The result is exactly the same as above. The jump address, now for the negative case is 0 and the result of subtraction is 0. The machine halts.  

This unusual symmetrical feature, much in the spirit of the general symmetrical nature of balanced ternary arithmetics, may help to understand better ternary programming and even, perhaps, to perform some automated code manipulatioms.

# Remarks

## 1

Note that unlike SUBLEQ, another esoteric one instruction machine, which also served as a source of inspiration for this project, branching in Nutes is decided **before** the subtraction. This allows to achieve the signwise symmetry explained below. SUBLEQ is known to be a practical computation model. A generalized version of SUBLEQ is easy to emulate in Nutes, as shown in the file instructions.lisp in this project.

Also note that the original values of the operands are preserved in case of halting and may be reused for interrupts, IO and other extentions.

## 2

If one wishes to switch the word width, use nutes-exp.lisp instead of nutes.lisp. However, it's highly recommended to use the current setting.

The suggested bare minimal word width is 6 trits or a tryte, a group of 6 trits, like the minimal addressable memory unit in the original Soviet machine. However, the length of machine words in Setun computers was 9 trits and its accumulator had 18 trits. 9 trits seems to be a somewhat practical minimum, 18 or 27 trits seem more suitable, and 36-trit words roughly correspond to today's 64-bit computers (36 trits are approximately 57 bits), while 48-trit words corespond to approximately 76 bits.

After a series of practical experiments, the 36-trit has been chosen as the most suitable, fitting nicely into 64-bit vectors. It also works well with the IO engine and other future extensions (work in progress, every detail of it must be defined in accordance to the direction/sign symmetry explained above and the principled lack of absolute addresses). 

## Why not 40 trits?

Although 40 trits ≈ 63.4 bits and fit almost perfectly into 64-bit arrays, the original creators of the ternary computers always used multiples and powers of 3 for their addressing and computation models. While 36 seems like a natural extension of the 18-trit accumulator of the original Soviet computers, 40-trit architecture seems unnatural and unaesthetic. Besides, Common Lisp (at least SBCL) works significantly faster with 63-bit fixnums rather than '(signed-byte 64).

# More about the balanced ternary

https://en.wikipedia.org/wiki/Balanced_ternary

# Copyright

Yoel Matveyev, 2017
