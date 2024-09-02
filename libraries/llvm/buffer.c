#ifndef EFFEKT_BUFFER_C
#define EFFEKT_BUFFER_C

#include "stdlib.h"
#include <stdint.h>
#include <string.h> // For memcopy

/** We represent buffers like positive types.
 *
 *  - The field `tag` contains offset (32bit) and length (32bit), which is equal to
 *    the tag (64bit) in Pos;
 *  - The field `obj` points to memory with the following layout:
 *
 *       +--[ Header ]--+--------------+
 *       | Rc  | Eraser | Contents ... |
 *       +--------------+--------------+
 *
 * The eraser is always a null pointer since it does not need to do anything.
 */

// TODO It may be performance-advantageous to implement this C file's semantics
// in LLVM, since the linker cannot realistically be asked to satisfactorily
// inline.
// Alternatively, one could drop gcc and combine Effekt-compiler-generated LLVM
// with clang-generated LLVM from this file's entire translation unit.


struct Pos c_buffer(const uint32_t offset, const uint32_t length, void* obj) {
    return (struct Pos) {
        .tag = ((uint64_t)offset << 32) | (uint32_t)length,
        .obj = obj,
    };
}

void c_buffer_erase_noop(void *envPtr) { (void)envPtr; }

uint64_t c_buffer_offset(const struct Pos buffer) {
    return (uint32_t)(buffer.tag >> 32); // Extract offset from the upper 32 bits
}

// Function to get the length from Pos
uint64_t c_buffer_length(const struct Pos buffer) {
    return (uint32_t)(buffer.tag & 0xFFFFFFFF); // Extract length from the lower 32 bits
}

uint8_t *c_buffer_bytes(const struct Pos buffer) {
    return (uint8_t *) (buffer.obj + sizeof(struct Header) + c_buffer_offset(buffer));
}

struct Pos c_buffer_construct(const uint64_t n, const uint8_t *data) {
    uint8_t *obj = (uint8_t *)malloc(sizeof(struct Header) + n * sizeof *obj);
    ASSERT_NON_NULL(obj);

    struct Header *header = (void*) obj;
    *header = (struct Header) { .rc = 0, .eraser = c_buffer_erase_noop, };

    // Copy data
    memcpy(obj + sizeof(struct Header), data, n);

    return c_buffer(0, n, obj);
}

struct Pos c_buffer_construct_zeroed(const uint64_t n) {
    uint8_t *zeroes = calloc(sizeof(struct Header) + n, sizeof *zeroes);
    ASSERT_NON_NULL(zeroes)

    struct Header *header = (void*) zeroes;
    *header = (struct Header) { .rc = 0, .eraser = c_buffer_erase_noop, };

    return c_buffer(0, n, zeroes);
}

struct Pos c_buffer_construct_uninitialized(const uint64_t n) {
    uint8_t *data = malloc(sizeof(struct Header) + n * sizeof *data);
    ASSERT_NON_NULL(data)

    struct Header *header = (void*) data;
    *header = (struct Header) { .rc = 0, .eraser = c_buffer_erase_noop, };

    return c_buffer(0, n, data);
}

struct Pos c_buffer_slice(struct Pos buffer, const uint64_t offset, const uint64_t length) {

    if ((c_buffer_offset(buffer) + offset + length) > c_buffer_length(buffer))
        return buffer;

    return c_buffer(c_buffer_offset(buffer) + offset, length, buffer.obj);
}

struct Pos c_buffer_clone(const struct Pos buffer) {
    return c_buffer_construct(c_buffer_length(buffer), c_buffer_bytes(buffer));
}

void c_buffer_copy(const struct Pos from, struct Pos to, uint64_t startFrom, uint64_t startTo, uint64_t length) {
    // Check bounds
    if ((startFrom + length > c_buffer_length(from)) || (startTo + length > c_buffer_length(to))) {
        return;
    }

    // Get pointers to the source and destination slices
    uint8_t *from_bytes = c_buffer_bytes(from) + startFrom;
    uint8_t *to_bytes = c_buffer_bytes(to) + startTo;

    // Copy bytes from `from` to `to`
    memcpy(to_bytes, from_bytes, length);
}


/**
 * Converts a buffer to a null-terminated string, handling zero bytes by encoding them as non-minimal UTF-8 sequences.
 *
 * Note: the returned pointer needs to be managed manually!
 *
 * @param buffer The buffer to convert.
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

    erasePositive(left);
    erasePositive(right);
    return concatenated;
}

struct Pos c_buffer_eq(const struct Pos left, const struct Pos right) {
    uint64_t left_len = c_buffer_length(left);
    uint64_t right_len = c_buffer_length(right);
    if (left_len != right_len) return BooleanFalse;
    for (uint64_t j = 0; j < left_len; ++j) {
        if (c_buffer_bytes(left)[j] != c_buffer_bytes(right)[j]) {
            erasePositive(left);
            erasePositive(right);
            return BooleanFalse;
        }
    }
    erasePositive(left);
    erasePositive(right);
    return BooleanTrue;
}

struct Pos c_buffer_substring(const struct Pos str, uint64_t start, uint64_t end) {
    const struct Pos substr = c_buffer_construct_zeroed(end - start);
    for (uint64_t j = 0; j < c_buffer_length(substr); ++j) {
        c_buffer_bytes(substr)[j] = c_buffer_bytes(str)[start+j];
    }
    erasePositive(str);
    return substr;
}

struct Pos c_buffer_show_Int(const Int n) {
    char str[24];
    sprintf(str, "%" PRId64, n);
    return c_buffer_construct_from_null_terminated_string(str);
}

struct Pos c_buffer_show_Byte(const uint8_t n) {
    char str[4];  // Byte values range from 0 to 255, 3 characters + null terminator
    sprintf(str, "%" PRIu8, n);
    return c_buffer_construct_from_null_terminated_string(str);
}

struct Pos c_buffer_show_Char(const uint64_t n) {
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

uint64_t c_buffer_index(const struct Pos buffer, const uint64_t index) {
    return c_buffer_bytes(buffer)[index];
}

void c_buffer_set(struct Pos buffer, const uint64_t index, uint8_t value) {
    c_buffer_bytes(buffer)[index] = value;
}

uint32_t c_buffer_character_at(const struct Pos buffer, const uint64_t index) {
    const uint8_t *bytes = c_buffer_bytes(buffer);
    uint8_t first_byte = bytes[index];
    uint32_t character = 0;

    uint32_t length = c_buffer_length(buffer);

    if (first_byte < 0x80) {
        // Single-byte character (0xxxxxxx)
        character = first_byte;
    } else if ((first_byte & 0xE0) == 0xC0) {
        // Two-byte character (110xxxxx 10xxxxxx)
        if (index + 1 < length) {
            character = ((first_byte & 0x1F) << 6) |
                        (bytes[index + 1] & 0x3F);
        }
    } else if ((first_byte & 0xF0) == 0xE0) {
        // Three-byte character (1110xxxx 10xxxxxx 10xxxxxx)
        if (index + 2 < length) {
            character = ((first_byte & 0x0F) << 12) |
                        ((bytes[index + 1] & 0x3F) << 6) |
                        (bytes[index + 2] & 0x3F);
        }
    } else if ((first_byte & 0xF8) == 0xF0) {
        // Four-byte character (11110xxx 10xxxxxx 10xxxxxx 10xxxxxx)
        if (index + 3 < length) {
            character = ((first_byte & 0x07) << 18) |
                        ((bytes[index + 1] & 0x3F) << 12) |
                        ((bytes[index + 2] & 0x3F) << 6) |
                        (bytes[index + 3] & 0x3F);
        }
    }

    erasePositive(buffer);
    return character;
}

#endif
