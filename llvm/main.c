#include <stdio.h>
#include <stdlib.h>


extern void effektMain();

void print(long int a) {
  printf("%ld\n", a);
}

int main() {
  effektMain();
}
