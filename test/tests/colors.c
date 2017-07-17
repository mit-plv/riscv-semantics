#define SCREEN_WIDTH 160
#define BYTES_PER_PIXEL 1
#define BYTES_PER_ROW (SCREEN_WIDTH * BYTES_PER_PIXEL)
#define SCREEN_MMAP_ADDR 0x2000

int toColor(int r, int g, int b) {
  return (r << 5) | (g << 2) | b;
}

void setPixel(int x, int y, int color) {
  int offset = BYTES_PER_PIXEL * x + BYTES_PER_ROW * y;
  *(char *)(SCREEN_MMAP_ADDR + offset) = color;
}

int main() {
  /* Display all available colors. */
  int x = 0;
  for (int g = 0; g < 8; g++) {
    for (int b = 0; b < 4; b++)  {
      for (int r = 0; r < 8; r++) {
        setPixel(x, 0, toColor(r, g, b));
        x++;
      }
    }
  }
}
