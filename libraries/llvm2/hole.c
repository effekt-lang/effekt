#ifndef EFFEKT_HOLE_C
#define EFFEKT_HOLE_C

#include <stdio.h>

void hole() {
    fprintf(stderr, "PANIC: Reached a hole in the program\n");
    exit(1);
}

#endif
