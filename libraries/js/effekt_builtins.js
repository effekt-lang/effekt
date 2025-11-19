/**
 * @typedef {Object} Unit
 * @property {true} __unit
 */

/**
 * Unit singleton value representing void/no value
 * @type {Unit}
 */
$effekt.unit = { __unit: true };

/**
 * Print a line to console
 * @param {string} str - String to print
 * @returns {Unit}
 */
$effekt.println = function(str) {
  console.log(str);
  return $effekt.unit;
};

/**
 * Convert value to display string
 * @param {*} obj - Object to show
 * @returns {string}
 */
$effekt.show = function(obj) {
  if (!!obj && !!obj.__reflect) {
    const meta = obj.__reflect();
    return meta.__name + "(" + meta.__data.map($effekt.show).join(", ") + ")";
  }
  else if (!!obj && obj.__unit) {
    return "()";
  } else {
    return "" + obj;
  }
};

/**
 * Check equality between two values
 * @param {*} obj1 - First value
 * @param {*} obj2 - Second value
 * @returns {boolean}
 */
$effekt.equals = function(obj1, obj2) {
  if (!!obj1.__equals) {
    return obj1.__equals(obj2);
  } else {
    return (obj1.__unit && obj2.__unit) || (obj1 === obj2);
  }
};

/**
 * Compare two values
 * @param {*} obj1 - First value
 * @param {*} obj2 - Second value
 * @returns {number} - -1 if obj1 < obj2, 0 if equal, 1 if obj1 > obj2
 */
$effekt.compare = function(obj1, obj2) {
  if ($effekt.equals(obj1, obj2)) { return 0; }

  if (!!obj1 && !!obj2) {
    if (!!obj1.__reflect && !!obj2.__reflect) {
      const tagOrdering = compare$prim(obj1.__tag, obj2.__tag);
      if (tagOrdering != 0) { return tagOrdering; }

      const meta1 = obj1.__reflect().__data;
      const meta2 = obj2.__reflect().__data;

      const lengthOrdering = compare$prim(meta1.length, meta2.length);
      if (lengthOrdering != 0) { return lengthOrdering; }

      for (let i = 0; i < meta1.length; i++) {
        const contentOrdering = $effekt.compare(meta1[i], meta2[i]);
        if (contentOrdering != 0) { return contentOrdering; }
      }

      return 0;
    }
  }

  return compare$prim(obj1, obj2);
};

/**
 * Throws an error for incomplete pattern matches
 * @throws {Error}
 * @returns {never}
 */
$effekt.emptyMatch = function() {
  throw "empty match";
};

/**
 * Placeholder for unimplemented code
 * @param {string} pos - Source position
 * @throws {Error}
 * @returns {never}
 */
$effekt.hole = function(pos) {
  throw pos + " not implemented yet";
};

/**
 * Internal primitive comparison
 * @param {number} n1
 * @param {number} n2
 * @returns {number}
 * @private
 */
function compare$prim(n1, n2) {
  if (n1 == n2) { return 0; }
  else if (n1 > n2) { return 1; }
  else { return -1; }
}