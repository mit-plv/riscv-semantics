int main() {
  asm("li t1,42\n"
      "csrrw zero,mstatus,t1\n"  // Set mstatus to 42
      "csrrwi t1,mstatus,13\n"  // Set t1 to mstatus, set mstatus to 13
      "csrrw t2,mstatus,zero\n"  // Set t2 to mstatus
      "sub a0,t1,t2\n" // a0 = t1 - t2 = 29
      "ret");
}
