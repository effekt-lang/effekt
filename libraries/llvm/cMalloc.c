#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>

// -----------------------------
// Strukturdefinition
// -----------------------------

/**
 * @brief Block-Struktur für die Freelist.
 *
 * Jeder freie Block zeigt auf den nächsten freien Block.
 */
typedef struct Block {
    struct Block* next;
} Block;

// -----------------------------
// Globale Variablen
// -----------------------------

static Block* freeList = NULL;            // Kopf der Freelist
static uint8_t* nextUnusedBlock = NULL;   // Zeiger auf nächsten unbenutzten Block
static const int blockSize = 128;     // Größe jedes Blocks (1KB)

// -----------------------------
// Initialisierung
// -----------------------------

/**
 * @brief Initialisiert den großen Speicherbereich (4GB).
 */
void cInitializeMemory(void) {
    size_t chunkSize = (size_t)4294967296ULL;  // 4GB

    // we allocate memory once from the os and use it for all effekt objects
    nextUnusedBlock = (uint8_t*)mmap(
        NULL,                          // Let the kernel pick the address
        chunkSize,           // Size of region
        PROT_READ | PROT_WRITE,        // Access permissions
        MAP_PRIVATE | MAP_ANONYMOUS,   // Not backed by a file
        -1,                            // No file descriptor
        0                              // Offset
    );
}

// -----------------------------
// Allokator
// -----------------------------

/**
 * @brief Einfacher Speicher-Allocator.
 *
 * Wenn die Freelist leer ist, nimmt er den nächsten Block im Chunk.
 * Wenn die Freelist nicht leer ist, nimmt er den ersten Eintrag daraus.
 *
 * @param size Ignoriert in diesem simplen Modell (wir geben immer 1KB).
 * @return void* Zeiger auf den allokierten Block.
 */
void* cMalloc(uint8_t size) {
    // 1. Falls Freelist leer ist → neuer Block
    if (freeList == NULL) {
        void* block = nextUnusedBlock;
        nextUnusedBlock += blockSize;
        return block;
    }


    // 2. Falls Freelist nicht leer ist → wiederverwenden
    Block* block = freeList;
    freeList = block->next;
    return (void*)block;
}

// -----------------------------
// Free-Funktion
// -----------------------------

/**
 * @brief Gibt einen Block zurück in die Freelist.
 *
 * @param ptr Zeiger auf den Block.
 */
void cFree(void* ptr) {
    if (!ptr) return;

    Block* block = (Block*)ptr;
    block->next = freeList;
    freeList = block;
}

void* cRealloc(void* ptr, uint8_t size) {
    printf("cRealloc: %p, %d\n", ptr, size);
    return realloc(ptr,size);
}

// -----------------------------
// Test / Demo
// -----------------------------

//int main(void) {
//    cInitializeMemory();
//
//    void* a = cMalloc(1024);
//    void* b = cMalloc(1024);
//    cFree(a);
//    void* c = cMalloc(1024);  // sollte a wiederverwenden
//    cFree(b);
//    cFree(c);
//
//    return 0;
//}
