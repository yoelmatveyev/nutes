# Introduction

In 1959, a group of scientists at the Moscow State University developed the unique experimental computer Setun, which used the balanced ternary numeral system instead of binary. The design has been was proven to be remarkably stable, efficient and elegant. Although the production was disbanded in 1965, major computer scientists, including Donald Knuth, still find the ternary computers exceptionally attractive.

Nutes (reverse of Setun) is a simple abstract machine, a ternary variety of a OISC (One Instruction Set Computer). Invented as a tribute to early Soviet computers,  this machine, also implementing an esoteric programming language, exhibits a number of unusual features, reminiscent of the Turing machines and cellular automata.

The machine is Turing-complete, because it is easy to implement other instructions known to be sutiable for OISC designs by a simple sequence of operations, e.g. "subtract and branch if negative".

# Description

The Nutes machine operates on either an infinite or circular memory tape divided into cells. Depending on the tape width, each cell contains either unbounded whole numbers or balanced ternary numbers composed of a particular number of trits, the ternary equivalent of bits. For example, each cell on a tape of a machine with the width of 3 may contain a number from -13 to +13 (from -3^2-3^1-3^0 to 3^2+3^1+3^0). The suggested practical minimal width is 6 or a tryte, a group of 6 trits or powers of 3 from 0 to 5, roughly comparable to a byte, like in the Russian machine.

The head of the machine reads the current cell and the two adjacent cells on both sides of the tape:

a b c d e

The cells a and e contain relative addresses to two other cells. The value of those cells, v1 and v2, are replaced, respectively, by v1-v2 and v2-v1. Depending on the sign of v1-v2, the head jumps to b cells, if the result is positive, to c cells, if the result is negative, or to d cells, if the result is negative. The computation continues until both c and the values pointed by a and e are 0, in which case the machine halts.

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

# More about the balanced ternary

https://en.wikipedia.org/wiki/Balanced_ternary

# Copyright

Yoel Matveyev, 2017
