const $builtins = (function() {

  function show(obj) {
    if (!!obj.tag) {
      return obj.tag + "(" + obj.data.map(show).join(", ") + ")"
    } else {
      return obj;
    }
  }
  function equals(obj1, obj2) {
    if (!!obj1.tag && !!obj2.tag) {
      if (obj1.tag != obj2.tag) return false;

      for (var i = 0; i < obj1.data.length; i++) {
        if (obj1.data[i] != obj2.data[i]) return false;
      }
      return true;
    } else {
      return obj1 == obj2;
    }
  }

  return {
    show: show,
    equals: equals,
    println: value => console.log(show(value)),
    concat: (s1, s2) => s1 + s2,
    mod: (n1, n2) => n1 % n2,
    mulDouble: (d1, d2) => d1 * d2,
    subDouble: (d1, d2) => d1 - d2
  };
})()

Object.assign($effekt, $builtins)
