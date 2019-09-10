# Introduction

In 1959, a group of scientists at the Moscow State University developed the unique experimental computer Setun, which used the balanced ternary numeral system instead of binary. The design has been was proven to be remarkably stable, efficient and elegant. Although the production was disbanded in 1965, major computer scientists, including Donald Knuth, still find the ternary computers exceptionally attractive.

Nutes (reverse of Setun) is a simple abstract machine, a ternary variety of a OISC (One Instruction Set Computer). Invented as a tribute to early Soviet computers,  this machine, also implementing an esoteric programming language, exhibits a number of unusual features, reminiscent of the Turing machines and cellular automata.

The machine is Turing-complete, because it is easy to implement other instructions known to be sutiable for OISC designs by a simple sequence of operations, e.g. "subtract and branch if negative".

# Description

The Nutes machine operates on either an infinite (theoretically) or circular memory tape divided into cells. Depending on the tape width, each cell contains either unbounded whole numbers or balanced ternary numbers composed of a particular number of trits, the ternary equivalent of bits. For example, each cell on a tape of a machine with the width of 3 may contain a number from -13 to +13 (from -3^2-3^1-3^0 to 3^2+3^1+3^0). The suggested practical minimal width is 6 or a tryte, a group of 6 trits or powers of 3 from 0 to 5, roughly comparable to a byte, like in the Russian machine.

The head of the machine reads the current cell and two other cells next to it on both sides of the tape:

x j y

The cell j contains a relative pointer to three other cells, which contain a series of jump addresses relative to the current cell:

j- j0 j+

The choice between the jumps is depending on the lowest digit of the sum of signs of x and y, which may also be viewed as the ternary analog of XOR, if the lowest digits are viewed as ternary logical values. The head jumps to j- number of cells, if the result is negative, to j+ number of cells, if the result is positive, or to j0 cells, if the result is zero. 

The computation continues until both j0 and the new values of the operands (x-y) are 0, in which case the machine halts. The original value of x or y (they are equal) are output and may be used for some additional functionality, such as interrupts.

The cells x and y contain relative pointers to two other cells, which contain the operands. The value of those cells, v1 and v2, are replaced, respectively, by v1-v2 and v2-v1. 

Note that unlike SUBLEQ, another esoteric one instruction machine, which inspired this project, branching in Nutes is decided **before** the subtraction. This allows to achieve the signwise symmetry explained below, as well to implement logic functions somewhat easier.

# Examples:

## 1. 

..1 1 -2 (Head)-> 4 -3 4 0 18 20

The head reads two operand pointers, both located by -2 cells to the left of its current location. The sign sum of the operands, both being 1, is actually **negative**: 1+1=1T (+-) in balanced ternary. The lowest digit is negative. The indirect branching pointer directs to the branching sequence of 0 18 20. Since the jump address for the negative case is 0 and the result of subtraction is 0, the machine halts:

..0 0 -2 4 -3 4 0 (Halted) 18 20

# Rationale

While the double negation and the triple branching based on the signs of the operands  may seem somewhat bizarre and redundant, this particular complex instruction exhibit an unusual symmetry, impossible in common real-life instruction sets. Because the cells do not have absolute addresses, the machine is truly Turing-complete, assuming that the tape is infinite. There is no essencial bias in favor of positive and negative numbers, as well as between the right and left direction of movement on the tape, although the sign and the direction are inherently linked to each other. Any program rewritten completely in reverse, while the sign of all its cells is reversed, performs exactly the same computation (except that all results, naturally, also have their sign reversed). Using the same example as above negated in reverse:

-20 -18 0 -4 3 (Head)-> -4 2 -1 -1

The sign sum is now **positive**, because -1-1=T1 (-+) in balanced ternary. The result is exactly the same as above. The jump address, now for the positive case is 0 and the result of subtraction is 0. The machine halts.  

This unusual symmetrical feature, much in the spirit of the general symmetrical nature of balanced ternary arithmetics, may help to understand better ternary programming and even, perhaps, to perform some automated code manipulatioms.

# More about the balanced ternary

https://en.wikipedia.org/wiki/Balanced_ternary

# Copyright

Yoel Matveyev, 2017
