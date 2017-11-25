int* CHAR_ADDR = 0x000fff4;

int getchar() {
  return *CHAR_ADDR;
}

int putchar(int c) {
  *CHAR_ADDR = c;
  return c;
}
