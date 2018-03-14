#include "../trap_handler.h"
#include "../mmio.h"

// https://stackoverflow.com/questions/20029892/what-is-the-colon-in-the-c-language-between-two-strings

void trap_handler() {
  // register char causechar asm ("x15");
  // asm volatile("csrr x15,mcause");
  register char causechar;
  asm volatile("csrr %0,mcause" : "=r"(causechar));

  causechar += 'A';
  putchar(causechar);
  putchar('\n');
}

int main() {
  // register trap handler
  asm volatile("csrrw zero,mtvec,%0" :: "r" (_trap_handler));
  // execute instruction which causes exception
  asm volatile("ebreak");
  // only runs after trap handler
  putchar('?');
  putchar('\n');
  return 0;
}
