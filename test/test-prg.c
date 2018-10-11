#include <stdio.h>

typedef struct {
  int x;
  char c;
} my_struct;

int adder(int x, int y) { return x + y; }

int main(int argc, char** argv) {
  my_struct st = { 4, 'd'};
  adder(1,2);
  return 0;
}
