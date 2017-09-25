# Introduction

In 1959, a group of scientists at the Moscow State University developed the unique experimental computer Setun, which used the balanced ternary numeral system instead of binary. The design has been was proven to be remarkably stable, efficient and elegant. Although the production was disbanded in 1965, major computer scientists, including Donald Knuth, still find the ternary computers exceptionally attractive.

Nutes (reverse of Setun) is a simple abstract machine, a ternary variety of a OISC (One Instruction Set Computer). Invented as a tribute to early Soviet computers,  this machine, also implementing an esoteric programming language, exhibits a number of unusual features, reminiscent of the Turing machines and cellular automata.

The machine is Turing-complete, because it is easy to implement other instructions known to be sutiable for OISC designs by a simple sequence of operations, e.g. "subtract and branch if negative".

# Description

The Nutes machine operates on either an infinite or circular memory tape divided into cells. Depending on the tape width, each cell contains either unbounded whole numbers or balanced ternary numbers composed of a particular number of trits, the ternary equivalent of bits. For example, each cell on a tape of a machine with the width of 3 may contain a number from -13 to +13 (from -3^2-3^1-3^0 to 3^2+3^1+3^0). The suggested practical minimal width is 6 or a tryte, a group of 6 trits or powers of 3 from 0 to 5, roughly comparable to a byte, like in the Russian machine.

The head of the machine reads the current cell and the two adjacent cells on both sides of the tape:

x jn jz jp y

The cells x and y contain relative addresses to two other cells. The value of those cells, v1 and v2, are replaced, respectively, by v1-v2 and v2-v1. Depending on the sign of v1+v2, the head jumps to jn cells, if the result is negative, to jz cells, if the result is zero, or to jp cells, if the result is positive. The computation continues until both jz and the values pointed by x and y are 0, in which case the machine halts. 

Note that depending on the tape width the results of the addition may be overflown and change its sign, e.g. 1000+1000 is -187, if the tape width is 6, in which case possible values of the cell are range from -362 to 362.

# Examples:

## 1. 

..4 0 (Head)-> 0 0 4 0 20..

The head reads two values, both located by 4 cells to the right of its current location. The values cancel each other and 20 is replaced by 0. The machine halts:

..4 0 (Head)-> 0 0 4 0 0.. (Halted)

## 2.

.. 100 0 0 0 -3 5 (Head)-> 5 5 11 5 5 5 -8 -13 0 0 0 -13 0 0 0 0 ..

.. 100 0 0 -100 -3 5 5 5 -6 11 5 (Head)-> 5 5 -8 -13 0 0 0 -13 0 0 0 0 ..

.. 100 0 0 -100 -3 5 5 5 -6 11 5 5 5 -8 -13 0 (Head)-> 0 0 -13 0 0 0 100 ..

.. 100 0 0 0 -3 5 5 5 -6 11 5 5 5 -8 -13 0 (Head)-> 0 0 -13 0 0 0 100 ..

The machine copies the value of a cell (100), removes the intermediary result (-100) and halts.

# Rationale

While the double negation and the triple branching based on addition may seem at first somewhat bizarre and redundant, this particular complex instruction exhibit an unusual symmetry, impossible in common real-life instruction sets. Because the cells do not have absolute addresses, the machine is truly Turing-complete, assuming that the tape is infinite. There is no essencial bias in favor of positive and negative numbers, as well as between the right and left direction of movement on the tape, although the sign and the direction are inherently linked to each other. Any program rewritten completely in reverse, while the sign of all its cells is reversed, performs exactly the same computation (except that all results, naturally, also have their sign reversed). Consider the following two possible scenarios:

Step 0: .. 0 0 2 -3 5 (Head)-> 10 -5 3 7 0 0 ..
Step 1: .. (Head)-> 0 0 5 -3 -5 10 -5 3 5 0 0 ..

Step 0: .. 0 0 -7 -3 5 (Head) -> -10 -5 3 -2 0 0 ..
Step 1: .. 0 0 -5 -3 5 -10 -5 3 5 0 (Head)-> 0 ..

Sign reversion is identical to direction reversion. This feature may help to understand better programming in balanced ternary and to perform automatic code manipulatioms.

# More about the balanced ternary

https://en.wikipedia.org/wiki/Balanced_ternary

# Copyright

Yoel Matveyev, 2017
