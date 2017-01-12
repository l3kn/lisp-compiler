#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>

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

#define pair_mask 0b00000111
#define pair_tag 0b00000001

#define vector_mask 0b00000111
#define vector_tag 0b00000101

typedef struct {
  void* rax; /* 0 scratch */
  void* rbx; /* 8 preserve */
  void* rcx; /* 16 scratch */
  void* rdx; /* 24 scratch */
  void* rsi; /* 32 preserve */
  void* rdi; /* 40 preserve */
  void* rbp; /* 48 preserve */
  void* rsp; /* 56 preserve */
} context;

// all scheme values are of type ptr
typedef unsigned long ptr;
extern ptr scheme_entry();

static void print_ptr(ptr x, char* heap) {
  if ((x & fixnum_mask) == fixnum_tag) {
    printf("%d", ((int)x) >> fixnum_shift);
  } else if ((x & char_mask) == char_tag) {
    printf("#\\%c", (char)(((int)x) >> char_shift));
  } else if ((x & pair_mask) == pair_tag) {
    ptr* cons = (ptr*)((x >> 3) << 3);
    printf("(");
    print_ptr(cons[0], heap);
    printf(" . ");
    print_ptr(cons[1], heap);
    printf(")");
  } else if ((x & vector_mask) == vector_tag) {
    ptr* vector = (ptr*)((x >> 3) << 3);
    int size = vector[0];
    printf("[ size: %d | ", size);
    for (int i = 0; i < size; i++) {
      print_ptr(vector[i+1], heap);
      if (i==(size-1)) {
        printf(" ]");
      } else {
        printf(", ");
      }
    }
  } else if (x == bool_true) {
    printf("#t");
  } else if (x == bool_false) {
    printf("#f");
  } else if (x == empty_list) {
    printf("()");
  } else {
    printf("#<unknown 0x%08x>", x);
  }
}

static char* allocate_protected_space(int size) {
  int page = getpagesize();
  int status;

  // Round to some size that is a multiple of page
  // and bigger or equal to the original size
  int aligned_size = ((size + page - 1) / page) * page;

  // Allocate the aligned size
  // and two additional pages
  char* p = mmap(0, aligned_size + 2 * page,
                 PROT_READ | PROT_WRITE,
                 MAP_ANONYMOUS | MAP_PRIVATE,
                 0, 0);
  if (p == MAP_FAILED) {
    printf("Failed to allocate");
    exit(EXIT_FAILURE);
  }

  // Protect the first and the last page
  // so the program crashes
  // if the stack over- or underflows
  status = mprotect(p, page, PROT_NONE);
  if (status != 0) {
    printf("Failed to protect the first page");
    exit(EXIT_FAILURE);
  }

  status = mprotect(p + page + aligned_size, page, PROT_NONE);
  if (status != 0) {
    printf("Failed to protect the last page");
    exit(EXIT_FAILURE);
  }

  // Add an offset of one page
  // because the first page is protected
  return (p + page);
}

static void deallocate_protected_space(char* p, int size) {
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;

  status = munmap(p - page, aligned_size + 2 * page);

  if (status != 0) {
    printf("Failed to deallocate");
    exit(EXIT_FAILURE);
  }
}

int main(int argc, char** argv) {
  int stack_size = (4 * 4 * 4096); // 16k 64bit cells
  char* stack_top = allocate_protected_space(stack_size);
  char* stack_base = stack_top + stack_size;

  int heap_size = (4 * 4 * 4096);
  char* heap = allocate_protected_space(heap_size);

  context ctxt;
  print_ptr(scheme_entry(&ctxt, stack_base, heap), heap);
  printf("\n");

  deallocate_protected_space(stack_top, stack_size);
  deallocate_protected_space(heap, heap_size);
  return 0;
}
