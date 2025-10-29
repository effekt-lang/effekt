#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>


/**
 * @brief Block-Struktur für die Freelist.
 *
 * Jeder freie Block zeigt auf den nächsten freien Block.
 */
typedef struct Block {
    struct Block* next;
} Block;

// Globale Variablen

static Block* freeList = NULL;            // Kopf der Freelist
static uint8_t* nextUnusedBlock = NULL;   // Zeiger auf nächsten unbenutzten Block
static uint8_t* endOfChunk = NULL;        // Ende des allokierten Speichers
static const int blockSize = 128;         // Größe jedes Blocks (128B)

/**
 * Initialisiert den großen Speicherbereich (4GB).
 */
void cInitializeMemory(void) {
    size_t chunkSize = (size_t)4294967296ULL;  // 4GB
    uint8_t* mem = (uint8_t*)malloc(chunkSize);
    if (!mem) {
        fprintf(stderr, "malloc() failed!\n");
        exit(1);
    }

    nextUnusedBlock = mem;
    endOfChunk = mem + chunkSize;
    printf("[init] Memory initialized: %p - %p\n", (void*)mem, (void*)endOfChunk);
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
void* acquire(uint8_t size) {
    // 1. Falls Freelist leer ist → neuer Block
    if (freeList == NULL) {
        if (nextUnusedBlock + blockSize > endOfChunk) {
            fprintf(stderr, "Out of memory!\n");
            return NULL;
        }

        void* block = nextUnusedBlock;
        nextUnusedBlock += blockSize;
        printf("[malloc] New block: %p\n", block);
        return block;
    }

    // 2. Falls Freelist nicht leer ist → wiederverwenden
    Block* block = freeList;
    freeList = block->next;
    printf("[malloc] Reusing block: %p\n", (void*)block);
    printf("[malloc] freeList: %p\n", (void*)freeList);
    return (void*)block;
}


/**
 * Gibt einen Block zurück in die Freelist.
 *
 * @param ptr Zeiger auf den Block.
 */
void release(void* ptr) {
    if (!ptr) return;

    Block* block = (Block*)ptr;
    block->next = freeList;
    freeList = block;

    printf("[free] Freed block: %p\n", ptr);
}

// @Deprecated
void* cRealloc(void* ptr, uint8_t size) {
    printf("cRealloc: %p, %d\n", ptr, size);
    return realloc(ptr,size);
}