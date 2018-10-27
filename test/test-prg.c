#include <stdio.h>

int global = 9;

int *gptr = &global;
int **gpp = &gptr;

const int const_global  = 10;

extern int ex;

typedef union {
  int x_int;
  double x_dub;
} my_union;

typedef struct {
  int x;
  char c;
} my_struct;

int *adder(int x, int y) {
  static int z = 0;
  z = x + y + z;
  return &z;
}

int main(int argc, char** argv) {
  my_struct st = { 4, 'd'};
  int b = *adder(1,2);
  printf("%d\n",b);
  return 0;
}
