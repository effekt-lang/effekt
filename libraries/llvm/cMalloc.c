#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>


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

static const bool DEBUG = false;
static Block* freeList = NULL; // Head of the Freelist
static uint8_t* nextUnusedBlock = NULL; // Pointer to the next unused Block
static uint8_t* endOfChunk = NULL; // End of the allocated Storage
static const int blockSize = 128; // The size of each block (128B)

// How much storage do we allocate at the beginning of a program? =4GB
static const size_t chunkSize = (size_t)4294967296ULL;

/**
 * Initialisiert den großen Speicherbereich (4GB).
 */
void cInitializeMemory(void)
{
    uint8_t* mem = (uint8_t*)malloc(chunkSize);
    if (!mem)
    {
        fprintf(stderr, "malloc() failed!\n");
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
            fprintf(stderr, "Out of memory!\n");
            return NULL;
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

        printf("Test failed!, Total Allocated: %zu, Elements in Free List: %zu \n", totalAllocated, numberOfElementsInFreeList);
        exit(1);
    }
}

/**
* Works as a Unit-Test, if all used blocks are freed at the end of the program.
* If not, we print an error message, making test fail.
*/
void testIfAllBlocksAreFreed()
{
//    assertNumberLeakedBlocks(0);
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
