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
}

/**
 * A simple allocator for the effekt slots that are created by newObject and deleted by eraseObject.
 * If we have an element in the to-do-list we can pop it and ensure that it is reusable by calling its eraser to erase the children.
 * Otherwise, bump allocate a new slot.
 */
void* acquire() {
    // 1. If there a slot to reuse...
    if (todoList != SENTINEL_SLOT) {

        // ...pop it from to-do-list
        Slot* reusedSlot = todoList;
        todoList = reusedSlot->next;

        return reusedSlot;
    }

    // 2. Fallback - we bump-allocate a new slot
    Slot* fresh = (Slot*)nextUnusedSlot;
    nextUnusedSlot += slotSize;

    return fresh;
}


/**
 * Pushes a slot on the top of the To-Do-List.
 */
void release(void* ptr) {
    Slot* slot = (Slot*)ptr;
    slot->next = todoList;
    todoList = slot;
}