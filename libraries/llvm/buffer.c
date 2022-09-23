#ifndef EFFEKT_BUFFER_C
#define EFFEKT_BUFFER_C


// TODO It may be performance-advantageous to implement this C file's semantics
// in LLVM, since the linker cannot realistically be asked to satisfactorily
// inline.


// Eight bytes for the reference counter.
// E.g. capacity could also be stored left of the data.
#define BUFFER_METADATA_WIDTH (8)

uint64_t c_buffer_length(const struct Pos buffer) {
    return buffer.tag;
}

// NOTE: This assumes a homogenously used byte order.
uint64_t *c_buffer_refcount(const struct Pos buffer) {
    return (uint64_t *) (buffer.obj - 8);
}

uint8_t *c_buffer_bytes(const struct Pos buffer) {
    return buffer.obj;
}


struct Pos c_buffer_construct(const uint64_t n, const uint8_t *data) {
    uint8_t *buf = malloc(BUFFER_METADATA_WIDTH + n * sizeof *buf);
    ASSERT_NON_NULL(buf)

    // reference count (a reference count of zero means one sole owner)
    for (uint64_t j = 0; j < BUFFER_METADATA_WIDTH; ++j)
        buf[j] = 0;

    // data
    for (uint64_t j = 0; j < n; ++j)
        buf[BUFFER_METADATA_WIDTH + j] = data[j];

    return (struct Pos) {
        .tag = n,
        .obj = BUFFER_METADATA_WIDTH + buf,
    };
}

void c_buffer_destruct(const struct Pos buffer) {
    free(buffer.obj - BUFFER_METADATA_WIDTH);
}


void c_buffer_refcount_increment(const struct Pos buffer) {
    if (DEBUG_REFCOUNT) { fprintf(stderr, "c_buffer_refcount_increment((struct Pos) { .tag = %" PRIu64 ", .obj = %p })\n", buffer.tag, buffer.obj); fflush(stderr); }
    (*c_buffer_refcount(buffer))++;
}

void c_buffer_refcount_decrement(const struct Pos buffer) {
    if (DEBUG_REFCOUNT) { fprintf(stderr, "c_buffer_refcount_decrement((struct Pos) { .tag = %" PRIu64 ", .obj = %p })\n", buffer.tag, buffer.obj); fflush(stderr); }
    if (!(*c_buffer_refcount(buffer))--)
        c_buffer_destruct(buffer);
}


struct Pos c_buffer_construct_zeroed(const uint64_t n) {
    uint8_t *zeroes = calloc(n, sizeof *zeroes);
    ASSERT_NON_NULL(zeroes)
    const struct Pos buffer = c_buffer_construct(n, zeroes);
    free(zeroes);
    return buffer;
}

void c_buffer_truncate(struct Pos buffer, const uint64_t n) {
    buffer.tag = n;
}

struct Pos c_buffer_copy(const struct Pos buffer) {
    return c_buffer_construct(c_buffer_length(buffer), c_buffer_bytes(buffer));
}


// incurs a malloc: the returned pointer needs to be managed
char *c_buffer_as_null_terminated_string(const struct Pos buffer) {
    // Zero runes are represented as non-minimal utf8 in the null-terminated
    // string. As such, a count reveals the necessary number of bytes.
    uint64_t zero_runes = 0;
    for (uint64_t j = 0; j < c_buffer_length(buffer); ++j)
        zero_runes += !c_buffer_bytes(buffer)[j];

    const uint64_t n = c_buffer_length(buffer) + zero_runes;
    uint8_t *buf = (uint8_t *) malloc((n+1) * sizeof *buf);
    ASSERT_NON_NULL(buf)

    uint64_t i = 0;
    for (uint64_t j = 0; j < c_buffer_length(buffer); ++j) {
        buf[i++] = c_buffer_bytes(buffer)[j];
        if (!buf[i]) {
            buf[i]   = 0xc0; // 0b110.00000
            buf[i++] = 0x80; // 0b10.000000
        }
    }

    // null-terminated
    buf[n] = '\00';
}

struct Pos c_buffer_construct_from_null_terminated_string(const char *data_nt) {
    uint64_t n = 0;
    while (data_nt[n++]);

    return c_buffer_construct(n, (uint8_t *) data_nt);
}


struct Pos c_buffer_concatenate(const struct Pos left, const struct Pos right) {
    const struct Pos concatenated = c_buffer_construct_zeroed(
        c_buffer_length(left) + c_buffer_length(right));
    for (uint64_t j = 0; j < c_buffer_length(concatenated); ++j)
        c_buffer_bytes(concatenated)[j]
            = j < c_buffer_length(left)
            ? c_buffer_bytes(left)[j]
            : c_buffer_bytes(right)[j - c_buffer_length(left)];
    return concatenated;
}


#endif
