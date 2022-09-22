#ifndef EFFEKT_BUFFER_C
#define EFFEKT_BUFFER_C


// TODO It may be performance-advantageous to implement this C file's semantics
// in LLVM, since the linker cannot realistically be asked to satisfactorily
// inline.




// Eight bytes for the reference counter.
// E.g. capacity could also be stored left of the data.
#define BUFFER_METADATA_WIDTH (8)

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

// NOTE: This assumes a homogenous byte order.
uint64_t* c_buffer_refcount(const struct Pos buffer) {
    return (uint64_t*) (buffer.obj - 8);
}

void c_buffer_refcount_increment(const struct Pos buffer) {
    (*c_buffer_refcount(buffer))++;
}

void c_buffer_refcount_decrement(const struct Pos buffer) {
    if (!(*c_buffer_refcount(buffer))--)
        c_buffer_destruct(buffer);
}


/*
#define MAX_BUFFER_GROWTH (4096)

struct Buffer {
    uint32_t len;
    uint32_t cap;
    uint8_t *buf;
};


bool c_buffer_is_valid(const struct Buffer *buffer) {
    return buffer->len <= buffer->cap && buffer->buf != NULL;
}

struct Buffer c_buffer_heapify(const uint32_t len, const char *text) {
    uint8_t *buf = (uint8_t *) malloc(len * sizeof *text);
    if (buf == NULL)
        return (struct Buffer) {};
    for (uint32_t j = 0; j != len; ++j)
        buf[j] = text[j];
    return (struct Buffer) {
        .len = len,
        .cap = len,
        .buf = buf,
    };
}

struct Buffer c_buffer_copy(const struct Buffer *buffer) {
    return c_buffer_heapify(buffer->len, buffer->buf);
}

bool c_buffer_grow(struct Buffer *buffer) {
    const uint32_t growth = buffer->cap <= MAX_BUFFER_GROWTH
        ? buffer->cap : MAX_BUFFER_GROWTH;
    uint8_t *buf = (uint8_t *) realloc(buffer->buf, buffer->cap+growth);
    if (buf == NULL)
        return false;

    buffer->cap += growth;
    buffer->buf = buf;
    return true;
}

bool c_buffer_grow_to(struct Buffer *buffer, const uint32_t mincap) {
    while (buffer->cap < mincap) {
        if (!c_buffer_grow(buffer))
            return false;
    }
    return true;
}

bool c_buffer_grow_to_exactly_fit(struct Buffer *buffer, const uint32_t cap) {
    if (buffer->cap <= cap)
        return true;

    uint8_t *buf = (uint8_t *) realloc(buffer->buf, cap);
    if (buf == NULL)
        return false;

    buffer->cap = cap;
    buffer->buf = buf;
    return true;
}

bool c_buffer_set_len(struct Buffer *buffer, const uint32_t len) {
    if (!c_buffer_grow_to_exactly_fit(buffer, len))
        return false;
    buffer->len = len;
    return true;
}

bool c_buffer_getenv(const struct Buffer *key, struct Buffer *value) {
    if (key->len >= 0xffffffff)
        return false;
    char *key_zt = (char *) malloc((key->len+1) * sizeof *key_zt);
    for (uint32_t j = 0; j < key->len; ++j)
        key_zt[j] = key->buf[j];
    key_zt[key->len] = '\00';

    const char *value_zt = getenv(key_zt);
    if (value_zt == NULL)
        return false;

    uint32_t value_len = 0;
    for (; value_zt[value_len] != '\00'; ++value_len)
        ;

    if (!c_buffer_set_len(value, value_len))
        return false;
    for (uint32_t j = 0; j < value->len; ++j)
        value->buf[j] = value_zt[j];

    return true;
}
*/


#endif
