function show$impl(obj) {
  if (!!obj && !!obj.__tag) {
    // constructor
    if (!!obj.__data) {
      return obj.__tag + "(" + obj.__data.map(show).join(", ") + ")"

    // record
    } else if (!!obj.__fields) {
      return obj.__tag + "(" + obj.__fields.map(field => show(obj[field])).join(", ") + ")"
    }
  } else if (obj === $effekt.unit) {
    return "()";
  } else {
    return "" + obj;
  }
}

function equals$impl(obj1, obj2) {
  if (!!obj1 && !!obj2 && !!obj1.__tag && !!obj2.__tag) {
    if (obj1.__tag != obj2.__tag) return false;

    // constructor
    if (!!obj.__data) {
      for (var i = 0; i < obj1.__data.length; i++) {
        if (!equals$impl(obj1.__data[i], obj2.__data[i])) return false;
      }
      return true;
    // record
    } else if (!!obj.__fields) {
      for (var i = 0; i < obj1.__fields.length; i++) {
        var field = obj1.__fields[i];
        if (!(equals$impl(obj1[field], obj2[field]))) return false;
      }
      return true;
    }
  } else {
    return obj1 === obj2;
  }
}

function println$impl(obj) {
  return $effekt.delayed(() => { console.log(show(obj)); return $effekt.unit; });
}

$effekt.unit = {}