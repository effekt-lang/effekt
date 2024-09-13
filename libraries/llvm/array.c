#ifndef EFFEKT_ARRAY_C
#define EFFEKT_ARRAY_C

/** We represent arrays like positive types.
 *  The tag is 0 and the obj points to memory with the following layout:
 *
 *   +--[ Header ]--+------+------------+
 *   | Rc  | Eraser | Size | Fields ... |
 *   +--------------+------+------------+
 */

void c_array_erase_fields(void *envPtr) {
  uint64_t *sizePtr = envPtr;
  struct Pos *dataPtr = envPtr + sizeof(uint64_t);
  uint64_t size = *sizePtr;
  for (uint64_t i = 0; i < size; i++) {
    erasePositive(dataPtr[i]);
  }
}

struct Pos c_array_new(const Int size) {
  void *objPtr = calloc(sizeof(struct Header) + sizeof(uint64_t) + size * sizeof(struct Pos), 1);
  struct Header *headerPtr = objPtr;
  uint64_t *sizePtr = objPtr + sizeof(struct Header);
  *headerPtr = (struct Header) { .rc = 0, .eraser = c_array_erase_fields, };
  *sizePtr = size;
  return (struct Pos) {
    .tag = 0,
    .obj = objPtr,
  };
}

Int c_array_size(const struct Pos arr) {
  uint64_t *sizePtr = arr.obj + sizeof(struct Header);
  return *sizePtr;
}

struct Pos c_array_get(const struct Pos arr, const Int index) {
  struct Pos *dataPtr = arr.obj + sizeof(struct Header) + sizeof(uint64_t);
  struct Pos element = dataPtr[index];
  sharePositive(element);
  return element;
}

struct Pos c_array_set(const struct Pos arr, const Int index, const struct Pos value) {
  struct Pos *dataPtr = arr.obj + sizeof(struct Header) + sizeof(uint64_t);
  struct Pos element = dataPtr[index];
  erasePositive(element);
  dataPtr[index] = value;
  return Unit;
}

#endif
