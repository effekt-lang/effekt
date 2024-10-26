#ifndef EFFEKT_BYTEARRAY_C
#define EFFEKT_BYTEARRAY_C

#include <string.h> // For memcopy

/** We represent bytearrays like positive types.
 *
 *  - The field `tag` contains the size
 *  - The field `obj` points to memory with the following layout:
 *
 *       +--[ Header ]--+--------------+
 *       | Rc  | Eraser | Contents ... |
 *       +--------------+--------------+
 *
 * The eraser does nothing.
 */


void c_bytearray_erase_noop(void *envPtr) { (void)envPtr; }

struct Pos c_bytearray_new(const Int size) {
  void *objPtr = malloc(sizeof(struct Header) + size);
  struct Header *headerPtr = objPtr;
  *headerPtr = (struct Header) { .rc = 0, .eraser = c_bytearray_erase_noop, };
  return (struct Pos) {
    .tag = size,
    .obj = objPtr,
  };
}

Int c_bytearray_size(const struct Pos arr) {
  return arr.tag;
}

uint8_t* c_bytearray_data(const struct Pos arr) {
  return arr.obj + sizeof(struct Header);
}

Byte c_bytearray_get(const struct Pos arr, const Int index) {
  Byte *dataPtr = arr.obj + sizeof(struct Header);
  Byte element = dataPtr[index];
  return element;
}

struct Pos c_bytearray_set(const struct Pos arr, const Int index, const Byte value) {
  Byte *dataPtr = arr.obj + sizeof(struct Header);
  dataPtr[index] = value;
  return Unit;
}

struct Pos c_bytearray_construct(const uint64_t n, const uint8_t *data) {
    struct Pos arr = c_bytearray_new(n);

    memcpy(c_bytearray_data(arr), data, n);

    return arr;
}

// Complex Operations

struct Pos c_bytearray_from_nullterminated_string(const char *data) {
    uint64_t n = 0;
    while (data[++n]);

    return c_bytearray_construct(n, (uint8_t*)data);
}

char* c_bytearray_into_nullterminated_string(const struct Pos arr) {
    uint64_t size = c_bytearray_size(arr);

    char* result = (char*)malloc(size + 1);

    memcpy(result, c_bytearray_data(arr), size);

    result[size] = '\0';

    // TODO we should erase the input
    return result;
}

// TODO do this in Effekt
struct Pos c_bytearray_show_Int(const Int n) {
    char str[24];
    sprintf(str, "%" PRId64, n);
    return c_bytearray_from_nullterminated_string(str);
}

// TODO do this in Effekt
struct Pos c_bytearray_show_Char(const uint64_t n) {
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

    return c_bytearray_from_nullterminated_string(str);
}

// TODO do this in Effekt
struct Pos c_bytearray_show_Byte(const Byte n) {
    char str[4];  // Byte values range from 0 to 255, 3 characters + null terminator
    sprintf(str, "%" PRIu8, n);
    return c_bytearray_from_nullterminated_string(str);
}

// TODO do this in Effekt
struct Pos c_bytearray_show_Double(const Double x) {
    char str[64]; // TODO is this large enough? Possibly use snprintf first
    sprintf(str, "%g", x);
    return c_bytearray_from_nullterminated_string(str);
}

// TODO do this in Effekt
struct Pos c_bytearray_concatenate(const struct Pos left, const struct Pos right) {
    const struct Pos concatenated = c_bytearray_new(c_bytearray_size(left) + c_bytearray_size(right));
    for (int64_t j = 0; j < c_bytearray_size(concatenated); ++j)
        c_bytearray_data(concatenated)[j]
            = j < c_bytearray_size(left)
            ? c_bytearray_data(left)[j]
            : c_bytearray_data(right)[j - c_bytearray_size(left)];

    erasePositive(left);
    erasePositive(right);
    return concatenated;
}

// TODO do this in Effekt
struct Pos c_bytearray_equal(const struct Pos left, const struct Pos right) {
    uint64_t left_size = c_bytearray_size(left);
    uint64_t right_size = c_bytearray_size(right);
    if (left_size != right_size) {
        erasePositive(left);
        erasePositive(right);
        return BooleanFalse;
    }
    for (uint64_t j = 0; j < left_size; ++j) {
        if (c_bytearray_data(left)[j] != c_bytearray_data(right)[j]) {
            erasePositive(left);
            erasePositive(right);
            return BooleanFalse;
        }
    }
    erasePositive(left);
    erasePositive(right);
    return BooleanTrue;
}

// TODO deprecate
struct Pos c_bytearray_substring(const struct Pos str, uint64_t start, uint64_t end) {
    const struct Pos substr = c_bytearray_new(end - start);
    for (int64_t j = 0; j < c_bytearray_size(substr); ++j) {
        c_bytearray_data(substr)[j] = c_bytearray_data(str)[start+j];
    }
    erasePositive(str);
    return substr;
}

// TODO deprecate
uint32_t c_bytearray_character_at(const struct Pos str, const uint64_t index) {
    const uint8_t *bytes = c_bytearray_data(str);
    uint8_t first_byte = bytes[index];
    uint32_t character = 0;

    uint32_t length = c_bytearray_size(str);

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

    erasePositive(str);
    return character;
}

#endif
