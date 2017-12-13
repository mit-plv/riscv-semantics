int getchar();
int putchar(int c);

int main() {
  char *s = "Hello, world!\n";
  char *p;
  for (p = s; p < s + 14; p++) putchar(*p);
  return 0;
}
