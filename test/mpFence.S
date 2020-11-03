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
  sw a4, 0(a0)
  fence
  sw a4, 0(a1)
  j out1
_startTh1:
  la a0, val
  la a1, flag
  lw a2, 0(a1)
  fence
  lw a3, 0(a0)


1:
  j 1b


val:
  .word 0x0
flag:
  .word 0x0
out1:
1:
  j 1b
