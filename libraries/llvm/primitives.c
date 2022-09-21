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
    printf("%ld\n", n);
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


// arithmetic

#define C_OP(T, OP_NAME, OP) \
    T c_ ## OP_NAME ## _ ## T ## _ ## T (const T x, const T y) { \
        return x OP y; }

// integer arithmetic
C_OP(Int, add, +)
C_OP(Int, sub, -)
C_OP(Int, mul, *)
C_OP(Int, div, /)
C_OP(Int, mod, %)

// floating-point arithmetic
C_OP(Double, add, +)
C_OP(Double, sub, -)
C_OP(Double, mul, *)
C_OP(Double, div, /)
// NOTE: requires linking against `-lm` and `#include <math.h>`
Double c_mod_Double_Double(Double x, Double y) { return fmod(x, y); }

#undef C_OP


#endif
