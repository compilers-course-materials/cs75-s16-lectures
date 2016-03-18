#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

extern int our_code_starts_here(int*) asm("our_code_starts_here");
extern void error() asm("error");
extern int print(int val) asm("print");
extern int equal(int val1, int val2) asm("equal");

const int TRUE = 0xFFFFFFFF;
const int FALSE = 0x7FFFFFFF;

int ispair(int p) {
  return (p & 0x00000007) == 0x00000001;
}

int equal(int val1, int val2) {
  if(val1 == val2) {
    return TRUE;
  }
  else if(ispair(val1) && ispair(val2)) {
    int* val1p = (int*) (val1 - 1);
    int* val2p = (int*) (val2 - 1);

    int leq = equal(*val1p, *val2p);
    if(leq == FALSE) { return FALSE; }

    return equal(*(val1p + 1), *(val2p + 1));
  }
  else {
    return FALSE;
  }
}

void print_rec(int val) {
  if(val & 0x00000001 ^ 0x00000001) {
    printf("%d", val >> 1);
  }
  else if(val == 0xFFFFFFFF) {
    printf("true");
  }
  else if(val == 0x7FFFFFFF) {
    printf("false");
  }
  else if(ispair(val)) {
    int* valp = (int*) (val - 1);
    printf("(");
    print_rec(*valp);
    printf(", ");
    print_rec(*(valp + 1));
    printf(")");
  }
  else {
    printf("Unknown value: %#010x", val);
  }
}

int print(int val) {
  print_rec(val);
  printf("\n");
  return val;
}

void error(int i) {
  if (i == 0) {
    fprintf(stderr, "Error: comparison operator got non-number");
  }
  else if (i == 1) {
    fprintf(stderr, "Error: arithmetic operator got non-number");
  }
  else if (i == 2) {
    fprintf(stderr, "Error: if condition got non-boolean");
  }
  else if (i == 3) {
    fprintf(stderr, "Error: Integer overflow");
  }
  else {
    fprintf(stderr, "Error: Unknown error code: %d\n", i);
  }
  exit(i);
}

int main(int argc, char** argv) {
  int* HEAP = calloc(sizeof (int), 100000);
  int result = our_code_starts_here(HEAP);
  print(result);
  return 0;
}

