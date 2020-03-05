if (typeof define !== 'function') { var define = require('amdefine')(module) }
define([], function() {

  // Result -- Trampoline
  function Step(c, k) {
    return { isStep: true, c: c, k: k }
  }
  function trampoline(r) {
    var res = r
    while (res !== null && res !== undefined && res.isStep) {
      res = res.c.apply(res.k)
    }
    return res
  }

  // Lists / Pairs
  function Cons(head, tail) {
    return { head: head, tail: tail }
  }
  const Nil = null

  // TODO maybe we can implement copy-on-write for the fields?

  // Metacontinuations / Stacks
  // (frames: List<Frame>, fields: Dict, prompt: Int, tail: Stack) -> Stack
  function Stack(frames, fields, prompt, tail) {
    return { frames: frames, fields: fields, prompt: prompt, tail: tail }
  }
  function SubStack(frames, backup, prompt, tail) {
    return { frames: frames, backup: backup, prompt: prompt, tail: tail }
  }
  const EmptyStack = null;

  // (stack: Stack<A, B>, a: A) -> B
  function apply(stack, a) {
    var s = stack;
    while (true) {
      if (s === EmptyStack) return a;
      const fs = s.frames;
      if (fs === Nil) { s = s.tail; continue }
      const result = fs.head(a);
      s.frames = fs.tail;
      return Step(result, s)
    }
  }

  function Cell(init) {
    return {
      _value: init,
      value: function(v) {
        if (arguments.length === 0) {
          return this._value
        } else {
          this._value = v
        }
      }
    }
  }
  function backup(cells) {
    return cells.map(c => ({ cell: c, value: c.value() }))
  }
  function restore(b) {
    return b.map(c => { c.cell.value(c.value); return c.cell })
  }

  // (subcont: Stack, stack: Stack) -> Stack
  function pushSubcont(subcont, stack) {
    var sub = subcont;
    var s = stack;

    while (sub !== EmptyStack) {
      s = Stack(sub.frames, restore(sub.backup), sub.prompt, s)
      sub = sub.tail
    }
    return s;
  }

  function flatMap(stack, f) {
    if (stack === EmptyStack) { return Stack(Cons(f, Nil), [], null, stack) }
    var fs = stack.frames
    // it should be safe to mutate the frames field, since they are copied in the subcont
    stack.frames = Cons(f, fs)
    return stack
  }

  function splitAt(stack, p) {
    var sub = EmptyStack;
    var s = stack;

    while (s !== EmptyStack) {
      const currentPrompt = s.prompt
      sub = SubStack(s.frames, backup(s.fields), currentPrompt, sub)
      s = s.tail
      if (currentPrompt === p) { return Cons(sub, s) }
    }
    throw ("Prompt " + p + " not found")
  }

  function withState(init, f) {
    const cell = Cell(init)
    return Control(k => {
      k.fields.push(cell);
      return Step(f(cell), k)
    })
  }

  // Delimited Control
  function Control(apply) {
    const self = {
      apply: apply,
      run: () => trampoline(Step(self, EmptyStack)),
      then: f => Control(k => Step(self, flatMap(k, f))),
      state: f => self.then(init => withState(init, f))
    }
    return self
  }

  const pure = a => Control(k => apply(k, a))

  const delayed = a => Control(k => apply(k, a()))

  const shift = p => f => Control(k => {
    const split = splitAt(k, p)
    const localCont = a => Control(k =>
      Step(pure(a), pushSubcont(split.head, k)))
    return Step(f(localCont), split.tail)
  })

  const reset = p => c => Control(k => Step(c, Stack(Nil, [], p, k)))

  var _prompt = 1;

  function _while(c, body) {
    return c().then(b => b ? body().then(() => _while(c, body)) : pure(null))
  }

  function handle(cls) {
    const p = _prompt++;

    const caps = cls.map(cl => function() {
      const args = Array.from(arguments);
      return shift(p)(k => cl.apply(null, args.concat([k])))
    });
    return body => reset(p)(body.apply(null, caps))
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

  return {
    pure: pure,
    delayed: delayed,
    // no lifting for prompt based implementation
    lift: f => f,
    handle: handle,

    _if: (c, thn, els) => c ? thn() : els(),
    _while: _while,
    constructor: (_, tag) => function() {
      return { tag: tag, data: Array.from(arguments) }
    },
    match: (sc, clauses) => clauses[sc.tag].apply(null, sc.data),

    println: function (obj) { console.log(show(obj)) },
    show: show,
    equals: equals,
    concat: function (s1, s2) { return s1 + s2 },
    mod: function (n1, n2) { return n1 % n2 },
    mulDouble: function (d1, d2) { return d1 * d2 },
    subDouble: function (d1, d2) { return d1 - d2 }
  }
})
