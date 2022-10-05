#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define DEBUG_REFCOUNT (false)

#include "sanity.c"
#include "types.c"
#include "buffer.c"
#include "io.c"


extern void effektMain();

int main(int argc, char *argv[]) {
    effektMain();
}
