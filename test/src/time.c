#include "../trap_handler.h"
#include "../mmio.h"

#define DELAY_TIME 20000

int *MTIMECMP_ADDR = (int *)0x2004000;
int *MTIME_ADDR = (int *)0x200bff8;

int running = 1;

void trap_handler() {
  putchar('!');
  putchar('\n');
  running = 0;
}

int main() {
  // Setup the trap handler.
  asm volatile("csrrw zero,mtvec,%0" :: "r" (_trap_handler));
  // Enable machine interrupts.
  int mstatus = 1 << 3;
  asm volatile ("csrrw zero,mstatus,%0" :: "r" (mstatus));
  // Set a timer.
  *MTIMECMP_ADDR = *MTIME_ADDR + DELAY_TIME;
  // Enable machine timer interrupts.
  int mtie = 1 << 7;
  asm volatile("csrrw zero,mie,%0" :: "r" (mtie));
  int c = 97;
  int i = 0;
  while (running) {
    if (i % 160 == 0) {
      putchar(c);
      putchar('\n');
      c++;
      if (c > 122) c = 97;
    }
    i++;
  }
  putchar('.');
  putchar('\n');
  return 0;
}
