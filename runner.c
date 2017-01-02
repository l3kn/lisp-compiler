#include <stdio.h>

#define wordsize 4

#define fixnum_shift 2
#define fixnum_mask 0b11
#define fixnum_tag 0b00

#define char_shift 8
#define char_mask 0b11111111
#define char_tag 0b00001111

#define bool_false 0b00101111
#define bool_true  0b01101111

#define bool_mask  0b00011111
#define bool_tag   0b00001111

#define empty_list 0b00111111

// all scheme values are of type ptr
typedef unsigned int ptr;
extern ptr scheme_entry();

static void print_ptr(ptr x) {
  if ((x & fixnum_mask) == fixnum_tag) {
    printf("%d", ((int)x) >> fixnum_shift);
  } else if ((x & char_mask) == char_tag) {
    printf("#\\%c", (char)(((int)x) >> char_shift));
  } else if (x == bool_true) {
    printf("#t");
  } else if (x == bool_false) {
    printf("#f");
  } else if (x == empty_list) {
    printf("()");
  } else {
    printf("#<unknown 0x%08x>", x);
  }

  printf("\n");
}

int main(int argc, char** argv) {
  print_ptr(scheme_entry());
  return 0;
}
