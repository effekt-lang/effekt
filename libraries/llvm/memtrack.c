/**
 * df: Simple memory tracker in C
 *
 * Tracks all invocations of malloc, calloc and free and prints a report an effekt program if it contains any memory leak.
 */

#include <stdio.h>
#include <stdlib.h>
#include "cMalloc.h"

const bool debug = false;   // if we should print debug messages

// Here we track all allocations done by cMalloc, cCalloc and cFree
typedef struct Allocation
{
    void* ptr; // pointer returned by malloc
    size_t size; // allocated size (for debugging only)
    struct Allocation* next; // next entry
} Allocation;

static Allocation* allocList = NULL;    // Head of the list

// Track total allocations for summary
static size_t totalAllocated = 0;
static size_t totalFreed = 0;

// Internal helper: add allocation to alloclist
static void addAllocation(void* ptr, size_t size)
{
    Allocation* entry = malloc(sizeof(Allocation));
    entry->ptr = ptr;
    entry->size = size;
    entry->next = allocList;
    allocList = entry;
    totalAllocated += size;
}

// Internal helper: remove allocation from alloclist
void removeAllocation(void* ptr)
{
    Allocation** curr = &allocList;
    while (*curr)
    {
        if ((*curr)->ptr == ptr)
        {
            Allocation* toFree = *curr;
            totalFreed += toFree->size;
            *curr = toFree->next;
            free(toFree);
            return;
        }
        curr = &((*curr)->next);
    }
    if (debug) {
        // Pointer not found — double free or invalid free was called
        printf("Warning: cFree() was called on untracked pointer %p\n", ptr);
    }
}

// call normal malloc() of the C library and track the allocation
void* cMalloc(size_t size)
{
    void* ptr = malloc(size);
    if (!ptr)
    {
        printf("Allocation failed with %p\n", &size);
        exit(1);
    }
    addAllocation(ptr, size);
    if (debug) {
        printf("[cMalloc] New block: %p\n", ptr);
    }
    return ptr;
}

// call normal calloc() of the C library and track the allocation
void* cCalloc(size_t mmemb, size_t size)
{
    void* ptr = calloc(mmemb, size);
    if (!ptr)
    {
        printf("Allocation failed with %p\n", &size);
        exit(1);
    }
    addAllocation(ptr, size);
    if (debug) {
        printf("[cCalloc] New block: %p\n", ptr);
    }
    return ptr;
}

// call normal free() of the C library and track the allocation
void cFree(void* ptr)
{
    if (ptr == NULL)
        return;
    if (debug) {
        printf("[cFree] Freed block: %p\n", ptr);
    }
    removeAllocation(ptr);
    free(ptr);
}

// If there are any remaining allocations (potential leaks), we report all of them and terminate the program
void assertLeakFree(void)
{
    if (allocList != NULL)
    {
        printf("\n----- MEMORY LEAK REPORT -----\n");
        Allocation* curr = allocList;
        while (curr)
        {
            printf("LEAK: %p (%zu bytes) allocated\n",
                   curr->ptr, curr->size);
            curr = curr->next;
        }
        printf("------------------------------\n");
        printf("Total allocated: %zu bytes\n", totalAllocated);
        printf("Total freed:     %zu bytes\n", totalFreed);
        printf("Still allocated: %zu bytes\n", totalAllocated - totalFreed);
        printf("------------------------------\n");
    }
}

// Simple demo
 // int main(void)
 // {
 //     printf("Start memtrack demo\n");
 //
 //     char* a = cMalloc(10);
 //     int* b = cMalloc(sizeof(int) * 5);
 //     double* c = cMalloc(sizeof(double) * 3);
 //
 //     cFree(b);
 //     //cFree(a); // Intentionally leaked
 //     //cFree(c); // Intentionally leaked
 //
 //     assertLeakFree(); // Show leaks at end
 //
 //     return 0;
 // }
