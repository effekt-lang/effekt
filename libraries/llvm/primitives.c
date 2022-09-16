#ifndef EFFEKT_PRIMITIVES_C
#define EFFEKT_PRIMITIVES_C


// types

struct Pos {
    uint64_t tag; // type-local tag
    void *obj; // heap object
};

typedef int64_t Int;
typedef double Double;

static const struct Pos Unit = (struct Pos) { .tag = 0, .obj = NULL, };
static const struct Pos BooleanFalse = (struct Pos) { .tag = 0, .obj = NULL, };
static const struct Pos BooleanTrue = (struct Pos) { .tag = 1, .obj = NULL, };


// i/o

struct Pos c_println_Int(const Int n) {
    printf("%" PRId64 "\n", n);
    return Unit;
}

struct Pos c_println_Boolean(const struct Pos p) {
    printf("%s\n", p.tag ? "true" : "false");
    return Unit;
}

struct Pos c_println_Double(const Double x) {
    printf("%g\n", x);
    return Unit;
}

void c_println_String(const struct Pos pos) {
    const uint32_t len = pos.tag & 0xffffffff;
    const uint8_t *buf = (uint8_t *) pos.obj;

    for (uint32_t j = 0; j != len; ++j)
        putchar(buf[j]);
    putchar('\n');
}

// TODO consider
void c_println_Buffer(const struct Pos pos) {
    const uint32_t len = pos.tag & 0xffffffff;
    const uint8_t *buf = (uint8_t *) pos.obj;

    printf("[");
    for (uint32_t j = 0; j != len; ++j)
        printf("0x%02X, ", (int) buf[j]);
    printf("]\n");
}


// buffer (TODO It may be performance-advantageous to implement the following in LLVM.)

#define ASSERT_NON_NULL(PTR) if ((PTR) == NULL) { \
    fprintf(stderr, "*** MALLOC PANIC\n"); \
    fflush(stderr); \
    exit(1); }

struct Pos c_buffer_heapify(const uint32_t len, const uint8_t *utf8) {
    uint8_t *buf = malloc(len * sizeof *buf);
    ASSERT_NON_NULL(buf)
    for (uint32_t j = 0; j != len; ++j)
        buf[j] = utf8[j];
    return (struct Pos) {
        .tag = (((uint64_t) len) << 32) | ((uint64_t) len),
        .obj = buf,
    };
}

#endif
