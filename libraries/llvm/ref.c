#ifndef EFFEKT_REF_C
#define EFFEKT_REF_C

/** We represent references like positive types.
 *  The tag is 0 and the obj points to memory with the following layout:
 *
 *   +--[ Header ]--+------------+
 *   | Rc  | Eraser | Field      |
 *   +--------------+------------+
 */

void c_ref_erase_field(void *envPtr) {
  struct Pos *fieldPtr = envPtr;
  struct Pos element = *fieldPtr;
  erasePositive(element);
}

struct Pos c_ref_fresh(const struct Pos value) {
  void *objPtr = malloc(sizeof(struct Header) + sizeof(struct Pos));
  struct Header *headerPtr = objPtr;
  struct Pos *fieldPtr = objPtr + sizeof(struct Header);
  *headerPtr = (struct Header) { .rc = 0, .eraser = c_ref_erase_field, };
  *fieldPtr = value;
  return (struct Pos) {
    .tag = 0,
    .obj = objPtr,
  };
}

struct Pos c_ref_get(const struct Pos ref) {
  struct Pos *fieldPtr = ref.obj + sizeof(struct Header);
  struct Pos element = *fieldPtr;
  sharePositive(element);
  return element;
}

struct Pos c_ref_set(const struct Pos ref, const struct Pos value) {
  struct Pos *fieldPtr = ref.obj + sizeof(struct Header);
  struct Pos element = *fieldPtr;
  erasePositive(element);
  *fieldPtr = value;
  return Unit;
}

#endif
