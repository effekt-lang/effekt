#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <uv.h>

/**
 * @brief Block-Struktur für die Freelist.
 *
 * Jeder freie Block zeigt auf den nächsten freien Block.
 */
typedef struct Block
{
    struct Block* next;
} Block;

// Globale Variablen

const bool DEBUG = false;
static Block* freeList = NULL; // Head of the Freelist
static uint8_t* nextUnusedBlock = NULL; // Pointer to the next unused Block
static uint8_t* endOfChunk = NULL; // End of the allocated Storage
static const int blockSize = 64; // The size of each block (64B)

// How much storage do we allocate at the beginning of a program? =4GB
static const size_t chunkSize = (size_t)4294967296ULL;

void printFreeList(const Block *freeList) {
    printf("All Elements in Free List:\n");

    const Block *b = freeList;
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
 * Wenn die Freelist leer ist, nimmt er den nächsten Block im Chunk.
 * Wenn die Freelist nicht leer ist, nimmt er den ersten Eintrag daraus.
 */
void* acquire(uint8_t size)
{
    // 1. Falls Freelist leer ist → neuer Block
    if (freeList == NULL)
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

    // 2. Falls Freelist nicht leer ist → wiederverwenden
    Block* block = freeList;
    freeList = block->next;

    if (DEBUG) {
        printf("[acquire] Reusing block: %p\n", (void*)block);
    }
    return (void*)block;
}


/**
 * Gibt einen Block zurück in die Freelist.
 *
 * @param ptr Zeiger auf den Block.
 */
void release(void* ptr)
{
    if (!ptr) return;

    Block* block = (Block*)ptr;
    block->next = freeList;
    freeList = block;

    if (DEBUG) {
        printf("[release] Freed block: %p\n", ptr);
    }
}

/**
*
*/
void assertNumberLeakedBlocks(int expected) {
    // Count how many blocks are in the freelist
    size_t numberOfElementsInFreeList = 0;
    for (const Block* b = freeList; b != NULL; b = b->next) {
        numberOfElementsInFreeList++;
    }

    // Total number of blocks that were ever allocated
    const uint8_t* firstBlock = (endOfChunk - chunkSize); // the start of the chunk
    const size_t totalAllocated = (nextUnusedBlock - firstBlock) / blockSize;

    // Calculate the number of leaked blocks
    const size_t numberOfLeakedBlocks = totalAllocated - numberOfElementsInFreeList;

    // Report if there are any leaked blocks
    if (numberOfLeakedBlocks != expected) {
        printf("firstBlock: %p\n", (void*)firstBlock);
        printf("nextUnusedBlock: %p\n", (void*)nextUnusedBlock);

        printFreeList(freeList);

        // we print all remaining block addresses   // TODO df: Is commented out because it creates warning now
//        for (uint8_t* p = firstBlock; p < nextUnusedBlock; p += blockSize) {
//            bool inFreeList = false;
//            for (const Block* b = freeList; b != NULL; b = b->next) {
//                if ((void*)b == (void*)p) {
//                    inFreeList = true;
//                    break;
//                }
//            }
//            if (!inFreeList) {
//                printf("  still used: %p\n", p);
//            }
//        }

        printf("Test failed!, Total Allocated: %zu, Elements in Free List: %zu \n", totalAllocated, numberOfElementsInFreeList);
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
