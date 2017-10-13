int putchar(int c);

void exception_handler() {
  putchar('!');
  putchar('\n');
  int ret_addr;
  asm volatile("csrrw %0,mepc,zero\n"
               "jalr %0,4" :: "r" (ret_addr));
}

int main() {
  asm volatile("csrrw zero,mtvec,%0" :: "r" (exception_handler));
  asm volatile(".ascii \"\\0\\0\\0\\0\"");
  putchar('?');
  putchar('\n');
  for(;;);
}
