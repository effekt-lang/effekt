function show$impl(obj) {
  if (!!obj && !!obj.__tag) {
    return obj.__tag + "(" + obj.__data.map(show$impl).join(", ") + ")"
  } else if (!!obj && obj.__unit) {
    return "()";
  } else {
    return "" + obj;
  }
}

function equals$impl(obj1, obj2) {
  if (!!obj1 && !!obj2 && !!obj1.__tag && !!obj2.__tag) {
    if (obj1.__tag != obj2.__tag) return false;

    for (var i = 0; i < obj1.__data.length; i++) {
      if (!equals$impl(obj1.__data[i], obj2.__data[i])) return false;
    }
    return true;
  } else {
    return (obj1.__unit && obj2.__unit) || (obj1 === obj2);
  }
}

function println$impl(obj) {
  return $effekt.delayed(() => { console.log(show$impl(obj)); return $effekt.unit; });
}

$effekt.unit = { __unit: true }