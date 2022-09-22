#include <inttypes.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "sanity.c"
#include "types.c"
#include "buffer.c"
#include "io.c"


extern void effektMain();

int main(int argc, char *argv[]) {
    c_io_prepare_command_line_arguments(argc, argv);
    effektMain();
}
