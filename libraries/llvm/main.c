#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h> // **only** for strerror

#define DEBUG_REFCOUNT (false)
#include "sanity.c"
#include "types.c"
#include "buffer.c"
#include "io.c"


extern void effektMain();

int main(int argc, char *argv[]) {
    c_io_prepare_command_line_arguments(argc, argv);
    effektMain();
}
