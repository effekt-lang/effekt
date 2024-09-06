#ifndef EFFEKT_TYPES_C
#define EFFEKT_TYPES_C


typedef int64_t Int;
typedef double Double;

struct Header {
  uint64_t rc;
  void (*eraser)(void *);
};

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

struct StackValue;

typedef struct StackValue* Stack;


// Defined in rts.ll

extern void resume_Int(Stack, Int);
extern void resume_Pos(Stack, struct Pos);

extern void run(struct Neg);
extern void run_Int(struct Neg, Int);
extern void run_Pos(struct Neg, struct Pos);

// Reference counting primitives defined in LLVM
extern void eraseNegative(struct Neg);
extern void erasePositive(struct Pos);
extern void eraseStack(Stack);

extern void shareNegative(struct Neg);
extern void sharePositive(struct Pos);
extern void shareStack(Stack);

#endif
