int getchar();
int putchar(int c);

int running = 1;

void trap_handler() {
  putchar('?');
  putchar('\n');
  running = 0;
}

// Wrapper for C function.
// Saves and restores a0, uses mret.
void _trap_handler();
asm("_trap_handler:\n"
    "  csrw mscratch,a0\n"
    "  call trap_handler\n"
    "  csrrw a0,mepc,zero\n"
    "  addi a0,a0,4\n"
    "  csrrw zero,mepc,a0\n"
    "  csrr a0,mscratch\n"
    "  mret");

int main() {
  // Setup the trap handler.
  asm volatile("csrrw zero,mtvec,%0" :: "r" (_trap_handler));
  // Enable machine interrupts.
  int mstatus = 1 << 3;
  asm volatile ("csrrw zero,mstatus,%0" :: "r" (mstatus));
  // Enable machine external interrupts, in particular.
  int meie = 1 << 11;
  asm volatile("csrrw zero,mie,%0" :: "r" (meie));
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
