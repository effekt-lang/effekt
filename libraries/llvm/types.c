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

struct Buffer {
    // together these are 64bit and thus fit into the "tag" of Pos
    uint32_t offset;
    uint32_t length;

    void *data;
};

static const struct Pos Unit = (struct Pos) { .tag = 0, .obj = NULL, };
static const struct Pos BooleanFalse = (struct Pos) { .tag = 0, .obj = NULL, };
static const struct Pos BooleanTrue = (struct Pos) { .tag = 1, .obj = NULL, };

typedef struct Buffer String;


// Defined in rts.ll

extern void run(struct Neg);
extern void run_i64(struct Neg, int64_t);
extern void run_Pos(struct Neg, struct Pos);
extern struct Neg* allocNeg(struct Neg);

// Reference counting primitives defined in LLVM
extern void eraseNegative(struct Neg);
extern void erasePositive(struct Pos);

extern void shareNegative(struct Neg);
extern void sharePositive(struct Pos);

#endif
