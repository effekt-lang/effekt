// stdlib
const effekt = (function() {

    const id = a => a

    function Pure(value) {
      return value
    }

    // c: Control[A], k: Cont<A, B>
    function Impure(c, k) {
      return { isComputation: true, c: c, k: k }
    }
    function trampoline(r) {
      var res = r
      while (res.isComputation) {
        res = res.c.apply(res.k)
      }
      return res
    }

    // Cont<A, B> = A => Result<B>

    // [A] apply: forall R. Cont<A, R> -> Result<R>
    function Control(apply) {
      const self = {
        apply: apply,
        run: () => apply(id),
        then: f => Control(k => Impure(self, a => k(a) )) // f => Control(k => Impure(self, k.flatMap(f)))
      };
      return self;
    }

    // (Result<A>, A => Control[B]) => Result[B]
    function bind(res, f) {
        if (res.isComputation) {
            res.c
        }
    }

    var pure = a => Control(k => k(a))

    var lift = f => function() {
      const args = arguments;
      return control(k1 => f.apply(null, args).then(k1))
    }

    var _if = (c, thn, els) => c ? thn() : els()
    var _while = (c, body) => c()(b => b ? body().then(() => _while(c, body)) : pure(null))
    var constructor = (_, tag) => function() {
      return { tag: tag, data: Array.from(arguments) }
    }
    var match = (sc, clauses) => clauses[sc.tag].apply(null, sc.data)

    function handle(cls) {
      const caps = cls.map(cl => function() {
        const args = Array.from(arguments);
        return control(k => cl.apply(null, args.concat([k])))
      });
      return body => body.apply(null, caps).apply(pure)
    }

    return {
      pure: pure,
      lift: lift,
      handle: handle,
      _if: _if,
      _while: _while,
      constructor: constructor,
      match: match
    }
})()

function println(obj) {
  console.log(obj)
}

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

//var before = new Date(); show(handledTriple(100, 15).run()); var after = new Date(); after - before