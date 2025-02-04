#ifndef EFFEKT_PANIC_C
#define EFFEKT_PANIC_C

#include <stdio.h>

void hole() {
    fprintf(stderr, "PANIC: Reached a hole in the program\n");
    exit(1);
}

void duplicated_prompt() {
    fprintf(stderr, "PANIC: Continuation invoked itself\n");
    exit(1);
}

#endif
