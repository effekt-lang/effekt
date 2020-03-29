
function show$impl(obj) {
  if (!!obj && !!obj.tag) {
    return obj.tag + "(" + obj.data.map(show).join(", ") + ")"
  } else {
    return obj;
  }
}

function equals$impl(obj1, obj2) {
  if (!!obj1 && !!obj2 && !!obj1.tag && !!obj2.tag) {
    if (obj1.tag != obj2.tag) return false;

    for (var i = 0; i < obj1.data.length; i++) {
      if (obj1.data[i] != obj2.data[i]) return false;
    }
    return true;
  } else {
    return obj1 === obj2;
  }
}

function println$impl(obj) {
  console.log(show(obj))
}
