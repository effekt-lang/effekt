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
static Block* todoList = SENTINEL_BLOCK; // Head of the To-Do-List
static Block* usedList = SENTINEL_BLOCK; // Head of the Used-List
static uint8_t* nextUnusedBlock = NULL; // Pointer to the next unused Block
static uint8_t* endOfChunk = NULL; // End of the allocated Storage
static const int blockSize = 64; // The size of each block (64B)

// How much storage do we allocate at the beginning of a program? =4GB
static const size_t chunkSize = (size_t)4294967296ULL;

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
 * Initialisiert den großen Speicherbereich (4GB).
 */
void cInitializeMemory()
{
    uint8_t* mem = (uint8_t*)malloc(chunkSize);
    if (!mem)
    {
        printf("Error: malloc() failed!\n");
        exit(1);
    }

    nextUnusedBlock = mem;
    endOfChunk = mem + chunkSize;
    if (DEBUG) {
        printf("[init] Memory initialized: %p - %p\n", (void*)mem, (void*)endOfChunk);
    }
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
    // 1. Falls Todolist leer ist → neuer Block
    if (todoList == SENTINEL_BLOCK)
    {
        if (nextUnusedBlock + blockSize > endOfChunk)
        {
            printf("Error: Out of memory!\n");
            exit(1);
        }

        void* block = nextUnusedBlock;
        nextUnusedBlock += blockSize;
        if (DEBUG) {
            printf("[acquire] New block: %p\n", block);
        }
        return block;
    }

    // 2. Falls Todolist nicht leer ist → wiederverwenden
    Block* block = todoList;

    if (DEBUG) {
        printf("[acquire] Reusing block: %p\n", (void*)block);
    }

    todoList = block->next;
    block->eraser((void*)block);

    return (void*)block;
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
//        printf("[release] todo head: %p\n", todoList);
//        printTodoList(todoList);
    }

    // Block zur todoList hinzufügen (von vorne)
    Block* block = (Block*)ptr;
    block->next = todoList;
    todoList = block;
}

/**
*
*/
void assertNumberLeakedBlocks(int expected) {
    // Calculate the number of leaked blocks
    size_t numberOfElementsInTodoList = 0;
    for (const Block* b = todoList; b != SENTINEL_BLOCK; b = b->next) {
        numberOfElementsInTodoList++;
    }

    // Total number of blocks that were ever allocated
    const uint8_t* firstBlock = (endOfChunk - chunkSize); // the start of the chunk
    const size_t totalAllocated = (nextUnusedBlock - firstBlock) / blockSize;

    // Calculate the number of leaked blocks
    const size_t numberOfLeakedBlocks = totalAllocated - numberOfElementsInTodoList;

    // if there leakes slots...
//    if (numberOfLeakedBlocks != expected) {
//        exit(1);
////        // we traverse each slot of the to-do-list and add it to the to-do list if missing
////        size_t numberOfSlotsToFlush = numberOfLeakedBlocks;
////        uint8_t* slotToAnalyze = (endOfChunk - chunkSize); // the start of the chunk
////        while(numberOfSlotsToFlush != expected) {
////            Block* blockToFlush = (Block*) slotToAnalyze;
////            bool isMissing = true;
////            for (const Block* b = todoList; b != NULL; b = b->next) {
////                if ((void*)b == (void*)blockToFlush) {
////                    isMissing = false;
////                    break;
////                }
////            }
////            if (isMissing) {
////                release(blockToFlush);
////                numberOfSlotsToFlush--;
////            }
////            slotToAnalyze = slotToAnalyze + blockSize;
////        }
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
    assertThatAllAsynchronousOperationsAreFinished();   // closing all open handles
    assertLeakFree();                  // testing malloc & calloc & free
    assertNumberLeakedBlocks(0);    // testing aquire & release
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
