#ifndef EFFEKT_BUFFER_C
#define EFFEKT_BUFFER_C

// NOTE: Buffers are NOT 0-terminated!

#include "stdlib.h"
#include <stdint.h>

// TODO It may be performance-advantageous to implement this C file's semantics
// in LLVM, since the linker cannot realistically be asked to satisfactorily
// inline.
// Alternatively, one could drop gcc and combine Effekt-compiler-generated LLVM
// with clang-generated LLVM from this file's entire translation unit.


// Eight bytes for the reference counter.
// E.g. capacity could later also be stored in the header.
#define BUFFER_HEADER_WIDTH (8)

uint64_t c_buffer_length(const struct Pos buffer) {
    return buffer.tag;
}

// NOTE: This assumes a homogenously used byte order.
uint64_t *c_buffer_refcount(const struct Pos buffer) {
    return (uint64_t *) (buffer.obj + 0);
}

uint8_t *c_buffer_bytes(const struct Pos buffer) {
    return (uint8_t *) (buffer.obj + BUFFER_HEADER_WIDTH);
}


struct Pos c_buffer_construct(const uint64_t n, const uint8_t *data) {
    uint8_t *obj = malloc(BUFFER_HEADER_WIDTH + n * sizeof *obj);
    ASSERT_NON_NULL(obj)

    // reference count (a reference count of zero means one sole owner)
    for (uint64_t j = 0; j < BUFFER_HEADER_WIDTH; ++j)
        obj[j] = 0;

    // data
    for (uint64_t j = 0; j < n; ++j)
        obj[BUFFER_HEADER_WIDTH + j] = data[j];

    return (struct Pos) {
        .tag = n,
        .obj = obj,
    };
}

void c_buffer_destruct(const struct Pos buffer) {
    free(buffer.obj);
}


void c_buffer_refcount_increment(const struct Pos buffer) {
    if (DEBUG_REFCOUNT) {
        fprintf(stderr, "c_buffer_refcount_increment((struct Pos) "
            "{ .tag = %" PRIu64 ", .obj = %p }): from %" PRIu64 "\n",
            buffer.tag, buffer.obj, *(c_buffer_refcount(buffer)));
        fflush(stderr);
    }

    (*c_buffer_refcount(buffer))++;
}

void c_buffer_refcount_decrement(const struct Pos buffer) {
    if (DEBUG_REFCOUNT) {
        fprintf(stderr, "c_buffer_refcount_decrement((struct Pos) "
            "{ .tag = %" PRIu64 ", .obj = %p }): from %" PRIu64 "\n",
            buffer.tag, buffer.obj, *(c_buffer_refcount(buffer)));
        fflush(stderr);
    }

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
    if (n > c_buffer_length(buffer))
        return;
    buffer.tag = n;
}

struct Pos c_buffer_copy(const struct Pos buffer) {
    return c_buffer_construct(c_buffer_length(buffer), c_buffer_bytes(buffer));
}


/**
 * Converts a Pos buffer to a null-terminated string, handling zero bytes by encoding them as non-minimal UTF-8 sequences.
 *
 * Note: the returned pointer needs to be managed manually!
 *
 * @param buffer The Pos buffer to convert.
 * @return A null-terminated string representing the contents of the buffer.
 */
char* c_buffer_as_null_terminated_string(const struct Pos buffer) {
    uint64_t length = c_buffer_length(buffer);
    uint64_t zero_runes = 0;
    uint8_t* bytes = c_buffer_bytes(buffer);

    // Count zero runes
    for (uint64_t j = 0; j < length; ++j) {
        zero_runes += !bytes[j];
    }

    // Allocate buffer for the null-terminated string
    uint64_t n = length + zero_runes;
    char *buf = (char *) malloc((n + 1) * sizeof(*buf));
    ASSERT_NON_NULL(buf)

    uint64_t i = 0;
    for (uint64_t j = 0; j < length; ++j) {
        buf[i++] = bytes[j];
        if (buf[i-1] == 0) {
            buf[i-1] = 0xc0; // 0b11000000
            buf[i++] = 0x80; // 0b10000000
        }
    }

    // Null-terminate the string
    buf[n] = '\0';

    return buf;
}

struct Pos c_buffer_construct_from_null_terminated_string(const char *data_nt) {
    uint64_t n = 0;
    while (data_nt[++n]);

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

struct Pos c_buffer_eq(const struct Pos left, const struct Pos right) {
    uint64_t left_len = c_buffer_length(left);
    uint64_t right_len = c_buffer_length(right);
    if(left_len != right_len) return BooleanFalse;
    for(uint64_t j = 0; j < left_len; ++j) {
        if(c_buffer_bytes(left)[j] != c_buffer_bytes(right)[j]) {
            return BooleanFalse;
        }
    }
    return BooleanTrue;
}

struct Pos c_buffer_substring(const struct Pos str, uint64_t start, uint64_t end) {
    const struct Pos substr = c_buffer_construct_zeroed(end - start);
    for (uint64_t j = 0; j < c_buffer_length(substr); ++j) {
        c_buffer_bytes(substr)[j] = c_buffer_bytes(str)[start+j];
    }
    return substr;
}

struct Pos c_buffer_show_Int(const Int n) {
    char str[24];
    sprintf(str, "%" PRId64, n);
    return c_buffer_construct_from_null_terminated_string(str);
}

struct Pos c_buffer_show_Char(const int32_t n) {
    char str[5] = {0};  // Max 4 bytes for UTF-8 + 1 for null terminator
    unsigned char *buf = (unsigned char *)str;

    if (n < 0x80) {  // 1-byte sequence
        buf[0] = (unsigned char)n;
    } else if (n < 0x800) {  // 2-byte sequence
        buf[0] = (unsigned char)(0xC0 | (n >> 6));
        buf[1] = (unsigned char)(0x80 | (n & 0x3F));
    } else if (n < 0x10000) {  // 3-byte sequence
        buf[0] = (unsigned char)(0xE0 | (n >> 12));
        buf[1] = (unsigned char)(0x80 | ((n >> 6) & 0x3F));
        buf[2] = (unsigned char)(0x80 | (n & 0x3F));
    } else if (n < 0x110000) {  // 4-byte sequence
        buf[0] = (unsigned char)(0xF0 | (n >> 18));
        buf[1] = (unsigned char)(0x80 | ((n >> 12) & 0x3F));
        buf[2] = (unsigned char)(0x80 | ((n >> 6) & 0x3F));
        buf[3] = (unsigned char)(0x80 | (n & 0x3F));
    }

    return c_buffer_construct_from_null_terminated_string(str);
}

struct Pos c_buffer_show_Double(const Double x) {
    char str[64]; // TODO is this large enough? Possibly use snprintf first
    sprintf(str, "%g", x);
    return c_buffer_construct_from_null_terminated_string(str);
}

uint64_t c_buffer_index(const struct Pos str, const uint64_t index) {
    return c_buffer_bytes(str)[index];
}

uint32_t c_buffer_character_at(const struct Pos buffer, const uint64_t index) {
    const uint8_t *bytes = c_buffer_bytes(buffer);
    uint8_t first_byte = bytes[index];
    uint32_t character = 0;

    if (first_byte < 0x80) {
        // Single-byte character (0xxxxxxx)
        character = first_byte;
    } else if ((first_byte & 0xE0) == 0xC0) {
        // Two-byte character (110xxxxx 10xxxxxx)
        if (index + 1 < buffer.tag) {
            character = ((first_byte & 0x1F) << 6) |
                        (bytes[index + 1] & 0x3F);
        }
    } else if ((first_byte & 0xF0) == 0xE0) {
        // Three-byte character (1110xxxx 10xxxxxx 10xxxxxx)
        if (index + 2 < buffer.tag) {
            character = ((first_byte & 0x0F) << 12) |
                        ((bytes[index + 1] & 0x3F) << 6) |
                        (bytes[index + 2] & 0x3F);
        }
    } else if ((first_byte & 0xF8) == 0xF0) {
        // Four-byte character (11110xxx 10xxxxxx 10xxxxxx 10xxxxxx)
        if (index + 3 < buffer.tag) {
            character = ((first_byte & 0x07) << 18) |
                        ((bytes[index + 1] & 0x3F) << 12) |
                        ((bytes[index + 2] & 0x3F) << 6) |
                        (bytes[index + 3] & 0x3F);
        }
    }

    return character;
}

#endif
