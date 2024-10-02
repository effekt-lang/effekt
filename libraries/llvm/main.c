#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define DEBUG_REFCOUNT (false)

#include "sanity.c"
#include "types.c"
#include "bytearray.c"
#include "io.c"
#include "hole.c"
#include "ref.c"
#include "array.c"


extern void effektMain();

int program_argc;
char** program_argv;

struct Pos c_get_arg(uint64_t idx) {
    if(idx < (uint64_t)program_argc) {
        return c_bytearray_from_nullterminated_string(program_argv[idx]);
    } else {
        return c_bytearray_new(0);
    }
}
uint64_t c_get_argc() {
    return program_argc;
}

int main(int argc, char *argv[]) {
    program_argc = argc;
    program_argv = argv;
    effektMain();
    uv_loop_t *loop = uv_default_loop();
    uv_run(loop, UV_RUN_DEFAULT);
    uv_loop_close(loop);
}
