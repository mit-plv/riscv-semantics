int main() {
  asm("li t1,42\n"
      "csrrw zero,mscratch,t1\n"  // Set mscratch to 42
      "csrrwi t1,mscratch,13\n"  // Set t1 to mscratch, set mscratch to 13
      "csrrw t2,mscratch,zero\n"  // Set t2 to mscratch
      "sub a0,t1,t2\n" // a0 = t1 - t2 = 29
      "ret");
}
