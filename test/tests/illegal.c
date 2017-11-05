int putchar(int c);

void trap_handler() {
  putchar('!');
  putchar('\n');
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
  asm volatile("csrrw zero,mtvec,%0" :: "r" (_trap_handler));
  asm volatile(".ascii \"\\0\\0\\0\\0\"");
  putchar('?');
  putchar('\n');
  return 0;
}
