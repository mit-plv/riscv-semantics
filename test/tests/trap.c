int getchar();
int putchar(int c);

int running = 1;

void trap_handler() {
  // TODO: Save registers using mscratch?
  putchar('?');
  putchar('\n');
  running = 0;
  void *ret_addr;
  asm volatile("csrrw %0,mepc,zero\n"
               "jalr %0" :: "r" (ret_addr));
}

int main() {
  // Setup the trap handler.
  asm volatile("csrrw zero,mtvec,%0" :: "r" (trap_handler));
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
