const $runtime = (function() {

  // Naming convention:
  // k ^= meta-continuation
  // c ^= computation (within the Control Monad)
  // p ^= prompt id / handler id
  // a ^= value
  // f ^= frame

  // Result -- Trampoline
  // (c: Control[A], k: Stack) -> Step
  function Step(c, k) {
    return { isStep: true, c: c, k: k }
  }

  // (r: Step) -> A
  function trampoline(r) {
    var res = r
    while (res !== null && res !== undefined && res.isStep) {
      res = res.c.apply(res.k)
    }
    return res
  }

  // Lists / Pairs. Only used immutably!
  // (head: A, tail: A) -> Cons[A]
  function Cons(head, tail) {
    return { head: head, tail: tail }
  }

  const Nil = null

  // Frame = A => Control[B]

  // Metacontinuations / Stacks
  // A metacontinuation is a stack of stacks.
  // (frames: List[Frame], fields: [Cell], prompt: Int, clauses: Clauses, tail: Stack) -> Stack
  function Stack(frames, fields, prompt, clauses, tail) {
    return { frames: frames, fields: fields, prompt: prompt, clauses: clauses, tail: tail }
  }

  // (frames: List[Frame], fields: [Cell], prompt: Int, clauses: Clauses, tail: Stack) -> Stack
  function SubStack(frames, backup, prompt, clauses, onUnwindData, tail) {
    return { frames: frames, backup: backup, prompt: prompt, clauses: clauses, onUnwindData: onUnwindData, tail: tail }
  }

  const EmptyStack = null;

  // |       |
  // | - T - |
  // |-------| - R
  //
  // onReturn: T -> Control[R]
  // onUnwind: () -> Control[S]
  // onRewind: S -> Control[Unit]
  function Clauses(onUnwind, onRewind, onReturn) {
    return {
      onUnwind: onUnwind, onRewind: onRewind, onReturn: onReturn
    }
  }

  const EmptyClauses = Clauses(null, null, null)

  // return a to stack
  // (stack: Stack<A, B>, a: A) -> Step<B>
  function apply(stack, a) {
    var s = stack;
    while (true) {
      if (s === EmptyStack) return a;
      const fs = s.frames;
      if (fs === Nil) {
        if (s.clauses.onReturn != null) {
          return Step(s.clauses.onReturn(a), s.tail)
        } else {
          s = s.tail;
          continue
        }
      }
      const result = fs.head(a);
      s.frames = fs.tail;
      return Step(result, s)
    }
  }

  // A cell is a mutable variable with an intial state.
  // (init: A) -> Cell[A]
  function Cell(init) {
    var _value = init;
    return {
      "op$get": function() {
        return $effekt.pure(_value)
      },
      "op$put": function(v) {
        _value = v;
        return $effekt.pure($effekt.unit)
      },
      backup: function() {
        var _backup = _value
        var cell = this;
        return () => { _value = _backup; return cell }
      }
    }
  }

  // (cells: [Cell]) -> [() => Cell]
  function backup(cells) {
    return cells.map(c => c.backup())
  }

  // (b: [() => Cell]) -> [Cell]
  function restore(b) {
    return b.map(c => c())
  }

  // Corresponds to a stack rewind.
  // (subcont: Stack, stack: Stack, c: Control[A]) -> Step
  function pushSubcont(subcont, stack, c, p, rewindedOn) {
    var sub = subcont;
    var s = stack;

    while (sub !== EmptyStack) {
      if (sub.clauses.onRewind != null && sub.prompt !== p && sub.prompt !== rewindedOn) {
        const remainder = sub
        // push sub onto s and execute the on rewind clause on the resulting stack.
        // Then remember to push sub.tail on the resulting stack k.
        return Step(
          sub.clauses.onRewind(sub.onUnwindData)
            .then(() => Control(k => pushSubcont(remainder, k, c, p, sub.prompt))),
          s
        )
      }
      s = Stack(sub.frames, restore(sub.backup), sub.prompt, sub.clauses, s)
      sub = sub.tail
    }
    return Step(c, s);
  }

  // Pushes a frame f onto the stack
  // (stack: Stack, f: Frame) -> Stack
  function flatMap(stack, f) {
    if (stack === EmptyStack) { return Stack(Cons(f, Nil), [], null, stack) }
    var fs = stack.frames
    // it should be safe to mutate the frames field, since they are copied in the subcont
    stack.frames = Cons(f, fs)
    return stack
  }

  // Corresponds to a stack unwind. Pops off stacks until the stack with prompt has been found
  // (stack: Stack, sub: Stack, p: Int, f: Frame) -> Step
  function splitAt(stack, sub, p, f) {
    var sub_ = sub;
    var s = stack;

    while (s !== EmptyStack) {
      const currentPrompt = s.prompt
      const onUnwindOp = s.clauses.onUnwind
      // do not execute the unwind operation if current handle is the needed one
      if (onUnwindOp != null && currentPrompt !== p) {
        const c = onUnwindOp().then(a =>
          Control(k => {
            sub_ = SubStack(s.frames, backup(s.fields), currentPrompt, s.clauses, a, sub_)
            return splitAt(k, sub_, p, f)
          })
        )
        return Step(c, s.tail)
      }

      sub_ = SubStack(s.frames, backup(s.fields), currentPrompt, s.clauses, null, sub_)
      if (currentPrompt === p) {
        const localCont = a => Control(k => pushSubcont(sub_, k, pure(a), p, 0))
        return Step(f(localCont), s.tail)
      }
      s = s.tail
    }
    throw ("Prompt " + p + " not found")
  }

  // (init: A, f: Frame) -> Control[B]
  function withState(init, f) {
    const cell = Cell(init)
    return Control(k => {
      k.fields.push(cell);
      return Step(f(cell), k)
    })
  }

  // Delimited Control "monad"
  function Control(apply) {
    const self = {
      // Control[A] -> Stack -> Step[Control[B], Stack]
      apply: apply,
      // Control[A] -> A
      run: () => trampoline(Step(self, Stack(Nil, [], toplevel, EmptyClauses, EmptyStack))),
      // Control[A] -> (A -> Control[B]) -> Control[B] 
      // which corresponds to monadic bind
      then: f => Control(k => Step(self, flatMap(k, f))),
      // Control[A] -> (A -> Control[B]) -> Control[B]
      state: f => self.then(init => withState(init, f))
    }
    return self
  }

  // Given a continuation, return/apply a to it
  // A => Control[A]
  const pure = a => Control(k => apply(k, a))

  // Delays native JS side-effects during creation of the Control Monad.
  const delayed = a => Control(k => apply(k, a()))

  const shift = p => f => Control(k => {
    //TODO A name change of splitAt is probably in order
    return splitAt(k, null, p, f)
  })

  const callcc = f => Control(k => {
    return f(a => trampoline(apply(k, a)))
  })

  const abort = Control(k => $effekt.unit)

  const capture = f => {
    // [abort; f
    const action = () => f($effekt.unit).then(() => abort)
    return shift(toplevel)(k =>
      k({
        shouldRun: false,
        cont : () => k({ shouldRun: true, cont: action })
      })).then(a => a.shouldRun ? a.cont() : $effekt.pure(a.cont))
  }

  const reset = p => clauses => c => Control(k => Step(c, Stack(Nil, [], p, clauses, k)))

  const toplevel = 1;
  // A unique id for each handle
  var _prompt = 2;

  function _while(c, body) {
    return c().then(b => b ? body().then(() => _while(c, body)) : pure($effekt.unit))
  }

  function handle(handlers, onUnwind, onRewind, onReturn) {
    const p = _prompt++;

    // modify all implementations in the handlers to capture the continuation at prompt p
    const caps = handlers.map(h => {
      var cap = Object.create({})
      for (var op in h) {
        const impl = h[op];
        cap[op] = function() {
          // split two kinds of arguments, parameters of the operation and capabilities
          const args = Array.from(arguments);
          const arity = impl.length - 1
          const oargs = args.slice(0, arity)
          const caps = args.slice(arity)
          const r = shift(p)(k => impl.apply(null, oargs.concat([k])))
          if (caps.length > 0) {
            return r.then(f => f.apply(null, caps))
          }
          // resume(v)
          else {
            return r
          }
        }
      }
      return cap;
    });
    return body => reset(p)(Clauses(onUnwind, onRewind, onReturn))(body.apply(null, caps))
  }

  return {
    pure: pure,
    callcc: callcc,
    capture: capture,
    delayed: delayed,
    // no lifting for prompt based implementation
    lift: f => f,
    handle: handle,

    _if: (c, thn, els) => c ? thn() : els(),
    _while: _while,
    constructor: (_, tag) => function() {
      return { __tag: tag, __data: Array.from(arguments) }
    },

    hole: function() { throw "Implementation missing" }
  }
})()

var $effekt = {}

Object.assign($effekt, $runtime);

module.exports = $effekt