#include "../trap_handler.h"
#include "../mmio.h"

int trap_handler_ran = 0;
char result = 0;

void trap_handler() {
  // register char causechar asm ("x15");
  // asm volatile("csrr x15,mcause");
  register char causechar;
  asm volatile("csrr %0,mcause" : "=r"(causechar));

  causechar += 'a';
  putchar(causechar);
  result = 65; // the trap handler is knows how to do the multiplication
  trap_handler_ran = 1;
}

int main() {
  // register trap handler
  asm volatile("csrrw zero,mtvec,%0" :: "r" (_trap_handler));

  register char a = 5;
  register char b = 13;
  register char c;

  // write mul in assembly to make sure the compiler doesn't
  // constant propagate or do other stuff.
  // mul causes exception if M extension not available
  asm volatile("mul %0,%1,%2"
	       : "=r"(c)           // output register
	       : "r"(a), "r"(b));  // input registers

  if (trap_handler_ran) {
    c = result;
  }
  putchar(c); 
  putchar('\n');
  return 0;
}
