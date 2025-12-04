#include <sys/mman.h>

/**
 * @brief Slot-Struktur für die To-Do-List & Used-List.
 *
 * Jeder freie Slot zeigt auf den nächsten freien Slot.
 */
typedef struct Slot
{
    struct Slot* next;
    void (*eraser)(void *object);
} Slot;

static bool DEBUG = false;
static Slot* freeList = NULL; // Head of the free-List

static const int slotSize = 64; // The size of each chunk (128B)
static const size_t totalAllocationSize = (size_t)4294967296ULL;    // How much storage do we allocate at the beginning of a program? =4GB

/**
 * Initializes the memory for our effekt-objects that are created by newObject and deleted by eraseObject.
 */
void initializeMemory() {

    // we allocate memory once from the os and use it for all effekt objects
    freeList = (Slot*)mmap(
        NULL,                          // Let the kernel pick the address
        totalAllocationSize,           // Size of region
        PROT_READ | PROT_WRITE,        // Access permissions
        MAP_PRIVATE | MAP_ANONYMOUS,   // Not backed by a file
        -1,                            // No file descriptor
        0                              // Offset
    );

    if (DEBUG) {
        uint8_t* endOfChunk = (uint8_t*)freeList + totalAllocationSize;
        printf("[init] Memory initialized: %p - %p\n", (void*)freeList, (void*)endOfChunk);
    }
}

/**
 * A simple allocator for the effekt slots that are created by newObject and deleted by eraseObject.
 * If we have an element in the to-do-list we can pop it and ensure that it is reusable by calling its eraser to erase the children.
 * Otherwise, bump allocate a new slot.
 */
void* acquire() {
    // 1. If there a slot to reuse...
    if (freeList->next != NULL) {

        // ...pop it from to-do-list
        Slot* reusedSlot = freeList;
        freeList = reusedSlot->next;

        if (DEBUG) printf("[acquire] Reusing block: %p\n", (void*)reusedSlot);

        return reusedSlot;
    }

    // 2. Fallback - we bump-allocate a new slot
    Slot* fresh = (Slot*)freeList;
    Slot* next = (Slot*)((uint8_t*)fresh + slotSize);
    freeList = next;

    if (DEBUG) printf("[acquire] New block: %p\n", (void*)fresh);
    return fresh;
}

/**
 * Pushes a slot on the top of the To-Do-List.
 */
void pushOntoFreeList(void* ptr) {
    if (DEBUG) {
        printf("[pushOntoFreeList] Freed block: %p\n", ptr);
    }

    Slot* slot = (Slot*)ptr;
    slot->next = freeList;
    freeList = slot;
}

void test(void* ptr) {
    printf("test: %p\n", ptr);
}