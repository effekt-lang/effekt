#ifndef EFFEKT_PANIC_C
#define EFFEKT_PANIC_C

#include <stdio.h>

// TODO:
// this should _morally_ be using `stderr`, but we don't tee it in tests
// see PR #823 & issue #815 for context

__attribute__((cold))
void hole(const char* message) {
    printf("PANIC: %s not implemented yet\n", message);
    exit(1);
}

__attribute__((cold))
void duplicated_prompt() {
    printf("PANIC: Continuation invoked itself\n");
    exit(1);
}

#endif
