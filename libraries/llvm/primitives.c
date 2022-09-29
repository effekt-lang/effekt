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


#endif
