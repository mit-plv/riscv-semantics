int *CHAR_ADDR = 0xfff4;

int getchar() {
  return *CHAR_ADDR;
}

int putchar(int c) {
  *CHAR_ADDR = c;
  return c;
}
