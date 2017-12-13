#include "../trap_handler.h"
#include "../mmio.h"

void trap_handler() {
  putchar('!');
  putchar('\n');
}

int main() {
  asm volatile("csrrw zero,mtvec,%0" :: "r" (_trap_handler));
  asm volatile(".ascii \"\\0\\0\\0\\0\"");
  putchar('?');
  putchar('\n');
  return 0;
}
