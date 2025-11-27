#include <sys/mman.h>

static const uint8_t* freeList = NULL;
static const size_t totalAllocationSize = (size_t)4294967296ULL;    // How much storage do we allocate at the beginning of a program? =4GB

/**
 * Initializes the memory for our effekt-objects that are created by newObject and deleted by eraseObject.
 */
void initializeMemory() {

    // we allocate memory once from the os and use it for all effekt objects
    freeList = (uint8_t*)mmap(
        NULL,                          // Let the kernel pick the address
        totalAllocationSize,           // Size of region
        PROT_READ | PROT_WRITE,        // Access permissions
        MAP_PRIVATE | MAP_ANONYMOUS,   // Not backed by a file
        -1,                            // No file descriptor
        0                              // Offset
    );
}

void* acquire(int size) {
    void* ptr = (void*)freeList;
    freeList = freeList + size;
    return ptr;
}

void release(void* ptr) {
}