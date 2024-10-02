#ifndef EFFEKT_BYTEARRAY_C
#define EFFEKT_BYTEARRAY_C

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
  void *objPtr = calloc(sizeof(struct Header) + size, 1);
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

#endif
