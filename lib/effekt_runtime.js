const $runtime = (function() {

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

  // Frame = A => Control[B]

  // Metacontinuations / Stacks
  // (frames: List<Frame>, fields: [Cell], prompt: Int, tail: Stack) -> Stack
  function Stack(frames, fields, prompt, tail) {
    return { frames: frames, fields: fields, prompt: prompt, tail: tail }
  }
  function SubStack(frames, backup, prompt, tail) {
    return { frames: frames, backup: backup, prompt: prompt, tail: tail }
  }
  const EmptyStack = null;

  // (stack: Stack<A, B>, a: A) -> Step<B>
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
      run: () => trampoline(Step(self, Stack(Nil, [], toplevel, EmptyStack))),
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

  const callcc = f => Control(k => {
    return f(a => trampoline(apply(k, a)))
  })

  const reset = p => c => Control(k => Step(c, Stack(Nil, [], p, k)))

  const toplevel = 1;
  var _prompt = 2;

  function _while(c, body) {
    return c().then(b => b ? body().then(() => _while(c, body)) : pure(null))
  }

  function handle(handlers) {
    const p = _prompt++;

    // modify all implementations in the handlers to capture the continuation at prompt p
    const caps = handlers.map(h => {
      var cap = Object.create({})
      for (var op in h) {
        const impl = h[op];
        cap[op] = function() {
          const args = Array.from(arguments);
          return shift(p)(k => impl.apply(null, args.concat([k])))
        }
      }
      return cap;
    });
    return body => reset(p)(body.apply(null, caps))
  }

  return {
    pure: pure,
    callcc: callcc,
    delayed: delayed,
    // no lifting for prompt based implementation
    lift: f => f,
    handle: handle,

    _if: (c, thn, els) => c ? thn() : els(),
    _while: _while,
    constructor: (_, tag) => function() {
      return { tag: tag, data: Array.from(arguments) }
    }
  }
})()

Object.assign($effekt, $runtime);
