# Introduction

In 1959, a group of scientists at the Moscow State University developed the unique experimental computer Setun, which used the balanced ternary numeral system instead of binary. The design has been was proven to be remarkably stable, efficient and elegant. Although the production was disbanded disbanded in 1965, major computer scientists, including Donald Knuth, still find the ternary computers exceptionally attractive.

Nutes (reverse of Setun) is a simple abstract machine, a ternary variety of a OISC (One Instruction Set Computer). Invented as a tribute to early Soviet computers,  this machine, implementing an esoteric programming language, exhibits a number of unusual features, reminiscent of the Turing machines and cellular automata.

# Description

The Nutes machine operates on either an infinite or circular memory tape divided into cells. Depending on the tape width, each cell contains either unbounded whole numbers or balanced ternary composed of a particular number of trits, the ternary equivalent of bits. For example, each cell on a tape of a machine with the width of 3 may be contain a number from -13 to +13 (from -3^2-3^1-3^0 to 3^2-3^1-3^0). The suggested practical minimal width is 6 or a tryte, a group of 6 trits of powers of 3 from 0 to 5, like in the Russian machine.

The head of the machine reads the current cell and the two adjacent cells on both sides of the tape:

a b c d e

The cells a and e contain relative addresses to two other cells. The value of those cells, v1 and v2, are replaced, respectively, by v1-v2 and v2-v1. Depending on the sign of v1-v2, the head jumps to b cells, if the result is positive, to c cells, if the result is negative, or to d cells, if the result is negative. The computation continues until both c and the values pointed by a and e are 0, in which case the machine halts.

# Copyright

Yoel Matveyev, 2017
