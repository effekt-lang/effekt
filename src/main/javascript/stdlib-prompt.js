const effekt = (function() {

  // Result -- Trampoline
  function Pure(value) {
    return value
  }
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

  // Lists / Pairs
  function Cons(head, tail) {
    return { head: head, tail: tail }
  }
  const Nil = null

  // Metacontinuations
  const EmptyCont = {
    apply: Pure,
    append: s => s,
    splitAt: c => { throw ("Prompt " + c + " not found") },
    map: f => EmptyCont.flatMap(a => pure(f(a))),
    flatMap: f => FramesCont(Cons(f, Nil), EmptyCont)
  };

  function FramesCont(frames, tail) {
    const self = {
      apply: a => {
        const result = frames.head(a)
        if (frames.tail === null) {
          return Impure(result, tail)
        } else {
          return Impure(result, FramesCont(frames.tail, tail))
        }
      },

      append: other => FramesCont(frames, tail.append(other)),
      splitAt: c => {
          const res = tail.splitAt(c);
          return Cons(FramesCont(frames, res.head), res.tail)
      },
      map: f => self.flatMap(a => pure(f(a))),
      flatMap: f => FramesCont(Cons(f, frames), tail)
    };
    return self
  }

  function PromptCont(p, tail) {
    const self = {
      apply: r => tail.apply(r),
      append: s => PromptCont(p, tail.append(s)),
      splitAt: c => {
        if (c === p) {
          return Cons(EmptyCont, tail)
        } else {
          const res = tail.splitAt(c)
          return Cons(PromptCont(p, res.head), res.tail)
        }
      },
      map: f => self.flatMap(a => pure(f(a))),
      flatMap: f => FramesCont(Cons(f, Nil), self)
    };
    return self
  }

  // Delimited Control
  function Control(apply) {
    const self = {
      apply: apply,
      run: () => trampoline(Impure(self, EmptyCont)),
      then: f => Control(k => Impure(self, k.flatMap(f)))
    }
    return self
  }

  const pure = a => Control(k => k.apply(a))

  const shift = p => f => Control(k => {
    const split = k.splitAt(p)
    const localCont = a => Control(k =>
      Impure(pure(a), split.head.append(PromptCont(p, k))))
    return Impure(f(localCont), split.tail)
  })

  const reset = p => c => Control(k => Impure(c, PromptCont(p, k)))

  var _prompt = 0;

  function handle(cls) {
    const p = _prompt
    _prompt = _prompt + 1;

    const caps = cls.map(cl => function() {
      const args = Array.from(arguments);
      return shift(p)(k => cl.apply(null, args.concat([k])))
    });
    return body => reset(p)(body.apply(null, caps))
  }

  return {
    pure: pure,
    // no lifting for prompt based implementation
    lift: f => f,
    handle: handle,

    _if: (c, thn, els) => c ? thn() : els(),
    _while: (c, body) => c()(b => b ? body().then(() => _while(c, body)) : pure(null)),
    constructor: (_, tag) => function() {
      return { tag: tag, data: Array.from(arguments) }
    },
    match: (sc, clauses) => clauses[sc.tag].apply(null, sc.data)
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