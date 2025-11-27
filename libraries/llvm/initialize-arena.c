#include <sys/mman.h>

// Initialize a 4 GiB arena via mmap.
// Does not work in LLVM properly, therefore it is done in C.
void* initializeArena(void) {
    return mmap(
        NULL,                         // no specific start adress
        4294967296ULL,                // 4 GiB
        PROT_READ | PROT_WRITE,       // Can read and write
        MAP_PRIVATE | MAP_ANONYMOUS,  // isolated, clean arena
        -1,                           // No file descriptor
        0                             // Offset
    );
}

void test(void* ptr) {
    printf("test: %p\n", ptr);
}