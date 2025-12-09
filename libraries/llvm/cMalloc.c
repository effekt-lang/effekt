#include <sys/mman.h>
#include <uv.h>

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

static uint8_t* nextUnusedSlot = NULL;
static uint8_t* endOfChunk = NULL;

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

    nextUnusedSlot = (uint8_t*)freeList;
    endOfChunk = nextUnusedSlot + totalAllocationSize;

    if (DEBUG) {
        printf("[init] Memory initialized: %p - %p\n", (void*)nextUnusedSlot, (void*)endOfChunk);
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
    nextUnusedSlot += slotSize;
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

/**
*   Checks if we have leaked slots. A slots is leaked, if it is not in the free-list.
*    If we have leaked slots, we flush all of them to our free list.
*/
void assertNumberLeakedSlots(int expected) {
    // Total number of slots that were ever allocated
    uint8_t* firstSlot = (endOfChunk - totalAllocationSize); // the start of the chunk
    const size_t totalAllocated = (nextUnusedSlot - firstSlot) / slotSize;

    if (DEBUG) {
    printf("[assertNumberLeakedSlots] firstSlot: %p, endOfChunk: %p\n", (void*)firstSlot, (void*)endOfChunk);
    printf("[assertNumberLeakedSlots] nextUnusedSlot: %p\n", nextUnusedSlot);
    printf("[assertNumberLeakedSlots] totalAllocated: %zu\n", totalAllocated);
    }

    size_t numberOfElementsInFreeList = 0;
    for (const Slot* slot = freeList; slot->next != NULL; slot = slot->next) {
        numberOfElementsInFreeList++;
    }

    if (numberOfElementsInFreeList - totalAllocated != expected) {
        printf("Error: Number of leaked slots is %zu, but should be %zu\n", numberOfElementsInFreeList, totalAllocated);
        exit(1);
    }
}

// If there are any open libuv handles or requests, this function will block until they are closed.
// This might happen if the program has asynchronous operations (e.g. read, write file) that are not finished.
// This is necessary to ensure that all memory is freed.
void assertThatAllAsynchronousOperationsAreFinished() {
    // 1. Complete all open libuv operations. This blocks until there are no more active handles or requests open.
    uv_run(uv_default_loop(), UV_RUN_DEFAULT);

    // 2. Now the loop can be closed.
    int close_result = uv_loop_close(uv_default_loop());
    if (close_result != 0) {
        printf("Error: uv_loop_close still detected some active handles\n");
        exit(1);
    }
}


/**
* Works as a Unit-Test, if all used blocks are freed at the end of the program.
* If not, we print an error message, making test fail.
*/
void testIfAllSlotsAreFreed()
{
    assertThatAllAsynchronousOperationsAreFinished();   // closing all open handles
    assertNumberLeakedSlots(0);    // testing acquire & release
}

java:suite://effekt.LLVMTests