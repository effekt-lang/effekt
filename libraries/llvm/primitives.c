#ifndef EFFEKT_PRIMITIVES_C
#define EFFEKT_PRIMITIVES_C

#include <inttypes.h>


struct Pos {
    uint64_t tag; // local tag
    void *obj; // heap object
};


void c_println_int64(const int64_t n) {
    printf("%ld\n", n);
}

void c_println_pos_boolean(const struct Pos p) {
    printf("%s\n", p.tag ? "true" : "false");
}

#endif
