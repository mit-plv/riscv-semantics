#include "encoding.h"

# define LREG lw
# define SREG sw
# define REGBYTES 4

.section ".text.init"
#  .text
#  .align 6
  .globl _startTh0
  .globl _startTh1
  .globl _start
_start:
_startTh0:
  la a0, val
  la a1, flag
  li a4, 1
  lw a4, 0(a0)
  sw a4, 0(a1)
  lw a2, 0(a1)
  j out1
_startTh1:
  la a0, val
  la a1, flag
  lw a2, 0(a1)
  sw a2, 0(a0)


1:
  j 1b


val:
  .word 0x12
flag:
  .word 0x41
out1:
1:
  j 1b

# Note: we observe twice the same execution because the last store generate an event twice.
# I am guessing because there are 2 prefixes but the two prefixes should be because of the 2 
# load words of thread 1, but 