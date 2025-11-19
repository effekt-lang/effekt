/**
 * @param {*} obj
 * @returns {string}
 */
$effekt.show = function(obj) {
  if (!!obj && !!obj.__reflect) {
    const meta = obj.__reflect()
    return meta.__name + "(" + meta.__data.map($effekt.show).join(", ") + ")"
  }
  else if (!!obj && obj.__unit) {
    return "()";
  } else {
    return "" + obj;
  }
}

/**
 * @param {*} obj1
 * @param {*} obj2
 * @returns {boolean}
 */
$effekt.equals = function(obj1, obj2) {
  if (!!obj1.__equals) {
    return obj1.__equals(obj2)
  } else {
    return (obj1.__unit && obj2.__unit) || (obj1 === obj2);
  }
}

/**
 * @param {number} n1
 * @param {number} n2
 */
function compare$prim(n1, n2) {
  if (n1 == n2) { return 0; }
  else if (n1 > n2) { return 1; }
  else { return -1; }
}

/**
 * @param {*} obj1
 * @param {*} obj2
 * @returns {-1 | 0 | 1} - -1 if obj1 < obj2, 0 if equal, 1 if obj1 > obj2
 */
$effekt.compare = function(obj1, obj2) {
  if ($effekt.equals(obj1, obj2)) { return 0; }

  if (!!obj1 && !!obj2) {
    if (!!obj1.__reflect && !!obj2.__reflect) {
      const tagOrdering = compare$prim(obj1.__tag, obj2.__tag)
      if (tagOrdering != 0) { return tagOrdering; }

      const meta1 = obj1.__reflect().__data
      const meta2 = obj2.__reflect().__data

      const lengthOrdering = compare$prim(meta1.length, meta2.length)
      if (lengthOrdering != 0) { return lengthOrdering; }

      for (let i = 0; i < meta1.length; i++) {
        const contentOrdering = $effekt.compare(meta1[i], meta2[i])
        if (contentOrdering != 0) { return contentOrdering; }
      }

      return 0;
    }
  }

  return compare$prim(obj1, obj2);
}

/**
 * @typedef {Object} Unit
 * @property {true} __unit
 */

/**
 * Unit singleton value (Effekt's `()`)
 * @type {Unit}
 */
$effekt.unit = { __unit: true };

/**
 * @param {string} str
 * @returns {Unit}
 */
$effekt.println = function println$impl(str) {
  console.log(str); return $effekt.unit;
}

/**
 * Throws an error for incomplete pattern matches
 * @throws {Error}
 */
$effekt.emptyMatch = function() { throw "empty match" }

/**
 * Placeholder for unimplemented code
 * @param {string} pos - Source position (already formatted)
 * @throws {Error}
 */
$effekt.hole = function(pos) { throw pos + " not implemented yet" }