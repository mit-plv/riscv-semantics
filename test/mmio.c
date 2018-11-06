int* PUT_ADDR = (int *)0x000fff0;
int* GET_ADDR = (int *)0x000fff4;

int getchar() {
  return *GET_ADDR;
}

int putchar(int c) {
  *PUT_ADDR = c;
  return c;
}
