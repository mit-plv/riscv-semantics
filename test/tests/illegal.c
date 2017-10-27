int putchar(int c);

void exception_handler() {
  putchar('!');
  putchar('\n');
  void *tmp;
  asm volatile("csrrw %0,mepc,zero\n"
               "addi %0,%0,4\n"
               "csrrw zero,mepc,%0\n"
               "mret" :: "r" (tmp));
}

int main() {
  asm volatile("csrrw zero,mtvec,%0" :: "r" (exception_handler));
  asm volatile(".ascii \"\\0\\0\\0\\0\"");
  putchar('?');
  putchar('\n');
  for(;;);
}
