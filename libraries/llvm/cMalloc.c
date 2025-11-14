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
static uint8_t* usedBitMask;
static size_t bitsetSize;
static uint8_t* nextUnusedBlock = NULL; // Pointer to the next unused Block
static uint8_t* endOfChunk = NULL; // End of the allocated Storage
static const int blockSize = 64; // The size of each block (64B)

// How much storage do we allocate at the beginning of a program? =4GB
static const size_t chunkSize = (size_t)4294967296ULL;

// Berechnet den Block-Index zu einer gegebenen Pointer‐Adresse.
// Dazu wird die Adresse in einen Abstand zum Chunk‐Anfang umgerechnet
// und anschließend durch die Blockgröße geteilt.
static inline size_t blockIndexFromPtr(const void* ptr) {
    return ((uint8_t*)ptr - (endOfChunk - chunkSize)) / blockSize;
}

// Markiert einen Block als „benutzt“, indem im Bitset das passende Bit
// gesetzt wird. index >> 3 wählt das Byte, index & 7 wählt das Bit darin.
static inline void usedSet(size_t index) {
    usedBitMask[index >> 3] |=  (1 << (index & 7));
}

// Markiert einen Block als „frei“, indem im Bitset das passende Bit
// gelöscht (auf 0 gesetzt) wird.
static inline void usedClear(size_t index) {
    usedBitMask[index >> 3] &= ~(1 << (index & 7));
}

// Prüft, ob ein bestimmter Block genutzt ist. Schiebt das relevante Bit
// auf Position 0 und liest dann aus, ob es 1 oder 0 ist.
static inline bool usedTest(size_t index) {
    return (usedBitMask[index >> 3] >> (index & 7)) & 1;
}

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

    // -----------------------------
    // Bitset für alle 4GB Blöcke bauen
    // -----------------------------
    size_t numberOfSlots = chunkSize / blockSize;
    size_t requiredBytesForRepresentingEachSlot = (numberOfSlots + 7) / 8;
    bitsetSize = requiredBytesForRepresentingEachSlot * 8;

    usedBitMask = calloc(requiredBytesForRepresentingEachSlot, 1);
    if (!usedBitMask) {
        printf("Error: bitset alloc failed!\n");
        exit(1);
    }

    if (DEBUG) {
        printf("[init] Memory initialized: %p - %p\n", (void*)mem, (void*)endOfChunk);
        printf("[init] Bitset: %zu bytes for %zu blocks\n", requiredBytesForRepresentingEachSlot, numberOfSlots);    // we require 8MB to store the bitmask
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

        // Markieren des Blocks als frei
        size_t idx = blockIndexFromPtr(block);
//        printf("idx: %zu\n", idx);
        usedSet(idx);

        return block;
    }

    // 2. Falls Todolist nicht leer ist → wiederverwenden
    Block* block = todoList;
    todoList = block->next;

    if (DEBUG) {
        printf("[acquire] Reusing block: %p\n", (void*)block);
    }

    // 3. Wir müssen die Children von dem Block erasen, um den Block neu befüllen zu können.
    block->eraser((void*)block);

    // 4. Markieren des Blocks als verwendet
    size_t idx = blockIndexFromPtr(block);
//    printf("idx: %zu\n", idx);
    usedSet(idx);

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

    // Markieren des Blocks als frei
    size_t idx = blockIndexFromPtr(block);
//    printf("idx: %zu\n", idx);
    usedClear(idx);
}

/**
*   Checks if we have leaked blocks. A block is leaked, if it is not in the to-do-list.
*    If we have leaked blocks, we flush all of them to our to-do list.
*/
void assertNumberLeakedBlocks(int expected) {
    // Total number of blocks that were ever allocated
    uint8_t* firstSlot = (endOfChunk - chunkSize); // the start of the chunk
    const size_t totalAllocated = (nextUnusedBlock - firstSlot) / blockSize;

    size_t numberOfElementsInTodoList = 0;
    for (const Block* b = todoList; b != SENTINEL_BLOCK; b = b->next) {
        numberOfElementsInTodoList++;
    }
    size_t numberOfLeakedBlocks = 0;

//    printf("requiredBytesForRepresentingEachSlot: %zu\n", bitsetSize);

    // Flush: turn all "used" (bit=1) blocks into todoList
    for (uint8_t* ptr = (uint8_t*)firstSlot; ptr < nextUnusedBlock; ptr+= blockSize) {
//        printf("block: %p\n", ptr);
        size_t idx = blockIndexFromPtr(ptr);
//        printf("idx: %zu\n", idx);
        bool isUsed = usedTest(idx);
//        printf("isUsed: %d\n", isUsed);
        if (isUsed) {
            release((Block*)ptr);
            numberOfLeakedBlocks++;
        }
    }

    // Assert that the number of leaked blocks is as expected
    if ((totalAllocated - numberOfElementsInTodoList - numberOfLeakedBlocks) != expected) {
        printf("Totally allocated: %zu | numberOfElementsInTodoList %zu | numberOfLeakedBlocks %zu \n", totalAllocated, numberOfElementsInTodoList, numberOfLeakedBlocks);
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
    assertNumberLeakedBlocks(0);    // testing acquire & release
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
