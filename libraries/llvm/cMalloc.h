#ifndef CMALLOC_H
#define CMALLOC_H

extern const bool DEBUG;

void cInitializeMemory();
void* acquire(uint8_t size);
void release(void* ptr);
void assertNumberLeakedBlocks(int expected);
void testIfAllBlocksAreFreed();


#endif