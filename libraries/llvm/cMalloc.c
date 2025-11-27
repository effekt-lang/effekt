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

/**
 * Sentinel-Slot: Fakes a block with a RC=1. It is used to mark the end of the To-Do-List.
 * But it is not a real heap-object, because it is not 8-byte aligned.
 */
#define SENTINEL_SLOT ((Slot*)1)

static bool DEBUG = false;
static Slot* freeList = NULL; // Head of the To-Do-List
static Slot* todoList = SENTINEL_SLOT; // Head of the To-Do-List

static uint8_t* nextUnusedSlot = NULL; // Pointer to the next unused Slot

static const int slotSize = 64; // The size of each chunk (128B)
static const size_t totalAllocationSize = (size_t)4294967296ULL;    // How much storage do we allocate at the beginning of a program? =4GB

/**
 * Initializes the memory for our effekt-objects that are created by newObject and deleted by eraseObject.
 */
void initializeMemory() {

    // we allocate memory once from the os and use it for all effekt objects
    nextUnusedSlot = (uint8_t*)mmap(
        NULL,                          // Let the kernel pick the address
        totalAllocationSize,           // Size of region
        PROT_READ | PROT_WRITE,        // Access permissions
        MAP_PRIVATE | MAP_ANONYMOUS,   // Not backed by a file
        -1,                            // No file descriptor
        0                              // Offset
    );

    if (DEBUG) {
        uint8_t* endOfChunk = nextUnusedSlot + totalAllocationSize;
        printf("[init] Memory initialized: %p - %p\n", (void*)nextUnusedSlot, (void*)endOfChunk);
    }
}

/**
 * A simple allocator for the effekt slots that are created by newObject and deleted by eraseObject.
 * If we have an element in the to-do-list we can pop it and ensure that it is reusable by calling its eraser to erase the children.
 * Otherwise, bump allocate a new slot.
 */
void* acquire() {
    // 1. If there a slot ...
    if (freeList != NULL) {

        // ...pop it from free-list
        Slot* reusedSlot = freeList;
        freeList = reusedSlot->next;
        if (DEBUG) printf("[acquire] Free block: %p\n", (void*)reusedSlot);

        return reusedSlot;
    }
    // 2. Slow: If there a slot to in the to-do...
    else if (todoList != SENTINEL_SLOT) {

        // ...pop it from to-do-list
        Slot* reusedSlot = todoList;
        todoList = reusedSlot->next;

        if (DEBUG) printf("[acquire] Todo block: %p\n", (void*)reusedSlot);

        // Call the eraser function on it. After that, it is safe to reuse it again.
        reusedSlot->eraser(reusedSlot);

        return reusedSlot;
    }

    // 3. Fallback - we bump-allocate a new slot
    Slot* fresh = (Slot*)nextUnusedSlot;
    if (DEBUG) printf("[acquire] New block: %p\n", (void*)fresh);
    nextUnusedSlot += slotSize;
    return fresh;
}


/**
 * Pushes a slot on the top of the Free-List.
 */
void release(void* ptr) {
    Slot* slot = (Slot*)ptr;
    if (DEBUG) printf("[release] block: %p\n", (void*)slot);

    slot->next = freeList;
    freeList = slot;
}

void pushToTodo(void* ptr) {
    Slot* slot = (Slot*)ptr;
    if (DEBUG) printf("[pushToTodo] block: %p\n", (void*)slot);

    slot->next = todoList;
    todoList = slot;
}

void test() {
    printf("test\n");
}

void myprint(void* ptr) {
    printf("MYPRINT: %p\n", ptr);
}