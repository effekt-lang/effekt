#ifndef EFFEKT_TYPES_C
#define EFFEKT_TYPES_C


typedef int64_t Int;
typedef double Double;

struct Pos {
    uint64_t tag; // type-local tag
    void *obj; // pointer into the heap
};

struct Neg {
    void *vtable;
    void *obj;
};

static const struct Pos Unit = (struct Pos) { .tag = 0, .obj = NULL, };
static const struct Pos BooleanFalse = (struct Pos) { .tag = 0, .obj = NULL, };
static const struct Pos BooleanTrue = (struct Pos) { .tag = 1, .obj = NULL, };

typedef struct Pos String;


#endif
