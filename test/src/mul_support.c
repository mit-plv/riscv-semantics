#include "../trap_handler.h"
#include "../mmio.h"


void trap_handler() {
  // register char causechar asm ("x15");
  // asm volatile("csrr x15,mcause");
  register char causechar;
  asm volatile("csrr %0,mcause" : "=r"(causechar));

  causechar += 'a';
  putchar(causechar);
  putchar('\n');
}

int main() {
  // register trap handler
  asm volatile("csrrw zero,mtvec,%0" :: "r" (_trap_handler));

  register char a = 5;
  register char b = 13;
  register char c = 'B';

  // write mul in assembly to make sure the compiler doesn't
  // constant propagate or do other stuff.
  // mul causes exception if M extension not available
  asm volatile("mul %0,%1,%2"
	       : "=r"(c)           // output register
	       : "r"(a), "r"(b));  // input registers

  // runs in any case, but if mul was unavailable, it will print the
  // old value 'B' instead of the new value 'A', and the trap handler
  // will not have run before
  putchar(c);
  putchar('\n');
  return 0;
}
