#include <pthread.h>
#include <stdio.h>
#include "types.c"

void c_erase_noop(void* obj) {
    // To disable unused parameter warning
    (void)obj;
}

// Defined in llvm
void* thread_wrapper(void*);

// Defined in llvm
void* actor_thread_wrapper(void*);

// A thread is a Pos with the thread Id stored in the tag
typedef struct Pos thread_t;

thread_t c_thread_start(struct Neg function) {
    void* wrap = malloc(sizeof(struct Header));

    struct Header* header = wrap;
    *header = (struct Header) { .rc = 0, .eraser = c_erase_noop };

    // We have to malloc here, because &function may be gone from the c stack
    // by the time the thread_wrapper reads it, leading to weirdness
    struct Neg* negPtr = malloc(sizeof(struct Neg));
    *negPtr = function; 

    pthread_t threadId;
    pthread_create(&threadId, NULL, &thread_wrapper, negPtr);

    struct Pos wrapped = (struct Pos) {
        .tag = threadId,
        .obj = wrap,
    };

    return wrapped;
}

void c_thread_join(thread_t thread) {
    pthread_join(thread.tag, NULL);
    erasePositive(thread);
}