# Introduction

In 1959, a group of scientists at the Moscow State University developed the unique experimental computer Setun, which used the balanced ternary numeral system instead of binary. The design has been was proven to be remarkably stable, efficient and elegant. Although the production was disbanded in 1965, major computer scientists, including Donald Knuth, still find ternary computers exceptionally attractive.

Nutes (reverse of Setun) is a simple abstract machine, a ternary variety of OISC (one instruction set computer). Invented as a tribute to early Soviet computers,  this machine, also implementing an esoteric programming language, exhibits a number of unusual features reminiscent of the Turing machines and other abstract automata. 

Another source of inspiration behind this projects is SUBLEQ, a one instruction set computer that uses "subtract and branch if negative" as its sole operation. SUBLEQ is trivial to emulate by Nutes.

The Nutes machine is truly Turing-complete (given an infinite tape). Arguably, Nutes is more practical than plain SUBLEQ without an interrpupt mechanism for computations of any complexity.


# Description

The Nutes machine operates on either an infinite (theoretically) or circular memory tape divided into cells. Each cell contains a word of a certain number of trits, ternary equivalents of bits.

The head of the machine reads the current cell and two other cells next to it on both sides of the tape:

x j y

The cell j contains a pointer to three other cells, which contain a series of jump addresses relative to the current cell:

j- j0 j+

The cells x and y contain pointers to two other cells, v1 and v2, which contain the actual operands. The pointers are relative to the current position of the machine head.

The choice between the jumps depends on the sum of signs of v1 and v2. The head jumps to j- number of cells, if the result is negative, to j+ number of cells, if the result is positive, or to j0 cells, if the result is zero. 

If the computation continues uninterrupted, the values of v1 and v2 are replaced, respectively, by v1-v2 and v2-v1.

If j0 is 0 and the sum sign of v1 and v2 is 0, the machine interrupts and passes the value of v1+v2 (trancated to the width) as an opcode for the IO engine. The engine splits the opcode into 3 equal parts, starting from the highest-value trit: flags, additional parameter and the operation code. If the tape width is not divisible by 3, the remaining 1 or 2 lowest trits are discarded. If the opcode is 0, the machine halts unconditionally (this prevents an obvious cause of infinite loops).

The interrupt engine evaluates its position on the tape (-3 or +3 from the position before the interrupt) as a jump address and two other cells next to it on both sides of the tape as two operands, similar to the main machine. The order of the operands depends in the sign of the opcode:

a1 j a2 (if the opcode is >0)

a2 j a1 (if the opcode is <0)

This arrangement preserves the sign/direction symmetry explained below. When defining new operations for the engine and designing new devices to be operated by it, one must be careful to preserve this symmetry.

Just like in the main machine, the cell j contains a pointer to three other cells, which contain a series of jump addresses relative to the current cell:

j- j0 j+

Depending on the sign returned by the operation, the interrupt engine jumps to j-, j0 or j+ cells and returns to the main machine from the interrupt.

# Assember and IO engine

Tapes may be programmed by a simple interactive assembler indended to be used in REPL and a basic set of composed instructions, such as addition, subtraction, left shift (identical in balanced ternary to multiplication by 3), moving, comparison, swapping etc.

Three operations are currently defined for the IO engine: 

Opcode  Operation

-1      Input

0       Output/Input

-1      Output

The second arguments of these operations addresses a stream or a device, which processes the first argument, the flags and the additional parameter of the opcode. Currently, the stream agrument is ignored and the lowest 2 trits of the additional parameter are interpreted as alphanumeric, ternary, balanced base-9. balanced base-27 and decimal IO for stdin/stdout.

Practical code examples include slow multiplication by repeated addition, finding consequent pairs of Fibonacci numbers (one of the most natural operations in our machine), factorial, basic input combined with "Hello World", generalized shift, sum of the operand's trits, trit by trit inversion, determining programmatically the tape's width.

# Examples:

## Tape

..1 1 -2 Head-> 4 -3 4 20 18 0

The head reads two operand pointers, both located by -2 cells to the left of its current location. The sign sum of the operands, both being 1, is positive (1+1). The indirect branching pointer directs to the branching sequence of 20 18 0. Since the jump address for the positive case is 0 and the result of subtraction is 0, the machine halts:

..0 0 -2 4 -3 Head(halted)-> 4 20 18 0

## IO

The following program produced by the assembler and made for tapes at least 36 trits wide asks a name for input and outputs "Hello World,[user]":

(9 16086946250976080 17943922394188172 14852728792888700 0 0 0 0 0 0 -1 1 36 3 3 3 -17 

Start-> 0

-8 -19 -6 -3 -12 0 -14 -23 -12 -9 -18 0 -20 -30 -18 -15 -24 0 -26 -37 -24 -21)

A block of 12 cells (0 0 0 0 0 0 -1 1 36 3 3 3) is predefined by the function (create-prg) for standard variables necessary for basic programming.

The numbers (16086946250976080 17943922394188172 14852728792888700) encode tryte strings "Name? ", "World,", "Hello ". The program calls the Output/Input operation to print "Name? " and replaces it by the user's input, and calls 3 times the Output operation, outputting the resulting 3 strings.

# Rationale

While the double negation and triple branching based on the signs of the operands may seem somewhat overcomplicated, this instruction exhibit an unusual symmetry, impossible in conventional instruction sets. Because the cells do not have absolute addresses, the machine is truly Turing-complete, assuming that the tape is infinite. There is no essencial bias in favor of positive and negative numbers, as well as between the right and left direction of movement on the tape, although the sign and the direction are inherently linked to each other. Any program rewritten completely in reverse with the sign of all its cells reversed as well, performs exactly the same computation (except that all results, naturally, also have their sign reversed). Using the same example as above negated in reverse:

0 -18 -20 -4 3 (Head)-> -4 2 -1 -1

The sign sum is now negative. The result is exactly the same as above. The jump address, now for the negative case is 0 and the result of subtraction is 0. The machine halts.

The following tape is the exact sign/direction reverse of the above "Hello World" program:

(21 24 37 26 0 24 15 18 30 20 0 18 9 12 23 14 0 12 3 6 19 8

Start-> 0 

17 -3 -3 -3 -36 -1 1 0 0 0 0 0 0 -14852728792888700 -17943922394188172 -16086946250976080 -9)

It works the same and outputs "̅H̅e̅l̅l̅o̅ ̅W̅o̅r̅l̅d̅,[user]". Overlined characters represent negative character values.

This unusual symmetrical feature, much in the spirit of the general symmetrical nature of balanced ternary arithmetics, may help to understand better ternary programming and, perhaps, perform some automated code manipulations.

# Notes

## 1

Unlike SUBLEQ, another esoteric one instruction machine, which also served as a source of inspiration for this project, branching in Nutes is decided **before** the subtraction. This allows to achieve the signwise symmetry explained below. SUBLEQ is known to be a practical computation model. A generalized version of SUBLEQ is easy to emulate in Nutes, as shown in the file instructions.lisp in this project.

Also note that the original values of the operands are preserved in case of halting and may be reused for interrupts, IO and other extentions.

## 2

After a series of practical experiments, the 36-trit has been chosen as the default tape width, fitting nicely into 64-bit vectors. Although up to 40 trits fit into 64 bits, 36 seems like a natural extension of the 18-trit accumulator of the original Soviet computers.

The impementation is optimized for machines with various widths up to 36. The suggested bare minimal tape width is 6 trits or a tryte, a group of 6 trits, like the minimal addressable memory unit in the original Soviet machine. Although, it is technically possible to implement a "Hello World" program even on the absolutely minimalistic 3-trit tape, which may serve as an excercise in esoretic computing.

As long as programs made for a lower width don't rely on a patricular width value, they remain back compatible with higher widths.

# More about the balanced ternary

https://en.wikipedia.org/wiki/Balanced_ternary

# Copyright

Yoel Matveyev, 2017
