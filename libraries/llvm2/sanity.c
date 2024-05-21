#ifndef EFFEKT_SANITY_C
#define EFFEKT_SANITY_C


#if CHAR_BIT != 8
#error Machine ought to have eight bits in a byte.
#endif


#define ASSERT_NON_NULL(PTR) if ((PTR) == NULL) { \
    fprintf(stderr, "*** MALLOC PANIC\n"); \
    fflush(stderr); \
    exit(1); }


#endif
