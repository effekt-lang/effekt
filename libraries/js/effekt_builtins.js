function show$impl(obj) {
  if (!!obj && !!obj.__reflect) {
    const meta = obj.__reflect()
    return meta.__name + "(" + meta.__data.map(show$impl).join(", ") + ")"
  }
  else if (!!obj && obj.__unit) {
    return "()";
  } else {
    return "" + obj;
  }
}

function equals$impl(obj1, obj2) {
  if (!!obj1.__equals) {
    return obj1.__equals(obj2)
  } else {
    return (obj1.__unit && obj2.__unit) || (obj1 === obj2);
  }
}

function println$impl(obj) {
  //return $effekt.delayed(() => { console.log(show(obj)); return $effekt.unit; });
  console.log(show$impl(obj)); return $effekt.unit;
}

$effekt.unit = { __unit: true }