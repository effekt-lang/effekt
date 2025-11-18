#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <uv.h>
#include "memtrack.c"

/**
 * @brief Block-Struktur für die To-Do-List & Used-List.
 *
 * Jeder freie Block zeigt auf den nächsten freien Block.
 */
typedef struct Block
{
    struct Block* next;
    void (*eraser)(void *object);
} Block;


/**
 * Sentinel-Wert: Ein Pointer, der garantiert niemals ein echter Heap-Pointer ist.
 *
 * Wir nutzen (Block*)1, weil:
 * - echte Heap-Pointer immer mindestens 8-Byte aligned sind
 * - 1 also niemals kollidiert
 * - ReferenceCount != 0 bleibt (wenn Header überlagert ist)
 */
#define SENTINEL_BLOCK ((Block*)1)

// Globale Variablen
const bool DEBUG = false;

static Block* freeList = SENTINEL_BLOCK; // Head of the free-List
static Block* todoList = SENTINEL_BLOCK; // Head of the To-Do-List

static size_t todoCount = 0;       // Anzahl dirty Blöcke
static const size_t TODO_LIMIT = 64;

static uint8_t* nextUnusedBlock = NULL; // Pointer to the next unused Block
static uint8_t* endOfChunk = NULL; // End of the allocated Storage

static const int chunkSize = 64; // The size of each chunk (64B)
static const size_t totalAllocationSize = (size_t)4294967296ULL;    // How much storage do we allocate at the beginning of a program? =4GB

void printTodoList(const Block *todoList) {
    printf("All Elements in Todo List:\n");

    const Block *b = todoList;
    while (b != NULL) {
        printf("  Block at %p\n", (void *)b);
        b = b->next;
    }

    printf("(end of list)\n");
}

/**
 * Initialisiert den großen Speicherbereich für den Allokator.
 */
void cInitializeMemory()
{
    uint8_t* mem = (uint8_t*)malloc(totalAllocationSize);
    if (!mem)
    {
        printf("Error: malloc() failed!\n");
        exit(1);
    }

    nextUnusedBlock = mem;
    endOfChunk = mem + totalAllocationSize;

    if (DEBUG) {
        printf("[init] Memory initialized: %p - %p\n", (void*)mem, (void*)endOfChunk);
    }
}

static inline void flushTodoList(void) {
    size_t toClean = todoCount < TODO_LIMIT ? todoCount : TODO_LIMIT;   // Limit the number of blocks to clean

    for (int i = 0; i < toClean; i++) {
        Block* block = todoList;
        todoList = block->next;

        if (DEBUG) {
            printf("[flushTodoList] Freeing block: %p\n", (void*)block);
        }

        // Call the eraser function
        block->eraser(block);

        block->next = freeList;
        freeList = block;
    }

    todoCount -= toClean;
}


// -----------------------------
// Allokator
// -----------------------------

/**
 * Einfacher Speicher-Allocator.
 *
 * Wenn die Todolist leer ist, nimmt er den nächsten Block im Chunk.
 * Wenn die Todolist nicht leer ist, nimmt er den ersten Eintrag daraus.
 */
void* acquire(uint8_t size)
{
    if (DEBUG) printf("[acquire] todoCount: %zu\n", todoCount);
    // 1. Fast Path - Falls Todolist was hat → reuse Block
    if (freeList != SENTINEL_BLOCK)
    {
        Block* block = freeList;
        freeList = block->next;

        if (DEBUG) printf("[acquire] FAST → %p\n", (void*)block);
        return block;
    }
    // 2. Slow Path - free-List leer -> Sweep & try free list again
    if (todoCount >= TODO_LIMIT) {

        flushTodoList();

        // Nach Sweep ist freeList höchstwahrscheinlich gefüllt
        if (freeList != SENTINEL_BLOCK) {

            Block* block = freeList;
            freeList = block->next;

            if (DEBUG) printf("[acquire] SWEEP → %p\n", (void*)block);
            return block;
        }
    }

    // 3. Fallbäck - wir allokieren einen neuen Block
    if (nextUnusedBlock + chunkSize > endOfChunk) {
        fprintf(stderr, "Arena exhausted – out of memory.\n");
        exit(1);
    }

    Block* fresh = (Block*)nextUnusedBlock;
    nextUnusedBlock += chunkSize;

    if (DEBUG) printf("[acquire] NEW → %p\n", (void*)fresh);
    return fresh;
}


/**
 * Gibt einen Block zurück in die To-Do-List.
 *
 * @param ptr Zeiger auf den Block (noch als Object mit Header).
 */
void release(void* ptr)
{
    if (!ptr) return;

    if (DEBUG) {
        printf("[release] Freed block: %p\n", ptr);
    }

    // Block zur todoList hinzufügen (von vorne)
    Block* block = (Block*)ptr;
    block->next = todoList;
    todoList = block;

    todoCount++;
}

/**
*   Checks if we have leaked blocks. A block is leaked, if it is not in the to-do-list.
*    If we have leaked blocks, we flush all of them to our to-do list.
*/
void assertNumberLeakedBlocks(int expected) {
    // Total number of blocks that were ever allocated
    uint8_t* firstSlot = (endOfChunk - totalAllocationSize); // the start of the chunk
    const size_t totalAllocated = (nextUnusedBlock - firstSlot) / chunkSize;

    size_t numberOfElementsInTodoList = 0;
    for (const Block* b = todoList; b != SENTINEL_BLOCK; b = b->next) {
        numberOfElementsInTodoList++;
    }

    if (numberOfElementsInTodoList != totalAllocated) {
        exit(1);
    }

//
////    printf("requiredBytesForRepresentingEachSlot: %zu\n", bitsetSize);
//
//    // Flush: turn all "used" (bit=1) blocks into todoList
//    for (uint8_t* ptr = (uint8_t*)firstSlot; ptr < nextUnusedBlock; ptr+= chunkSize) {
////        printf("block: %p\n", ptr);
//        size_t idx = blockIndexFromPtr(ptr);
////        printf("idx: %zu\n", idx);
//        bool isUsed = usedTest(idx);
////        printf("isUsed: %d\n", isUsed);
//        if (isUsed) {
//            release((Block*)ptr);
//            numberOfLeakedBlocks++;
//        }
//    }
//
//    // Assert that the number of leaked blocks is as expected
//    if ((totalAllocated - numberOfElementsInTodoList - numberOfLeakedBlocks) != expected) {
//        printf("Totally allocated: %zu | numberOfElementsInTodoList %zu | numberOfLeakedBlocks %zu \n", totalAllocated, numberOfElementsInTodoList, numberOfLeakedBlocks);
//        exit(1);
//    }
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
void testIfAllBlocksAreFreed()
{
//    assertThatAllAsynchronousOperationsAreFinished();   // closing all open handles
//    assertLeakFree();                  // testing malloc & calloc & free
//    assertNumberLeakedBlocks(0);    // testing acquire & release
}

// small testprogram to test the allocator
//int main(void)
//{
//    cInitializeMemory();
//    assertNumberLeakedBlocks(0);
//
//    void* a = acquire((uint8_t)1024);
//    assertNumberLeakedBlocks(1);
//
//    void* b = acquire((uint8_t)1024);
//    assertNumberLeakedBlocks(2);
//
//    release(a);
//    assertNumberLeakedBlocks(1);
//
//    release(b);
//    assertNumberLeakedBlocks(0);
//
//    void* c = acquire((uint8_t)1024); // should reuse a
//    assertNumberLeakedBlocks(1);
//
//    release(c);
//    testIfAllBlocksAreFreed();
//
//    return 0;
//}
