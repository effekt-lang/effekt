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

  // (frames: List[Frame], fields: [Cell], prompt: Int, tail: Stack) -> Stack
  function Stack(frames, fields, prompt, onReturn, tail) {
    return { frames: frames, fields: fields, prompt: prompt, onReturn: onReturn, tail: tail }
  }

  // (frames: List[Frame], fields: [Cell], prompt: Int, tail: Stack) -> Stack
  function SubStack(frames, backup, prompt, onReturn, tail) {
    return { frames: frames, backup: backup, prompt: prompt, onReturn: onReturn, tail: tail }
  }

  const EmptyStack = null;

  // Invariant: a Finalizer stack always occurs as either the tail of a Stack or as predecessor of a SubStack.
  // Only the top-level stack does not have a Finalizer stack.
  // 
  // |       |      
  // | - T - |
  // |-------| { onReturn: T -> Control[R]
  //                                        / onUnwind: () -> Control[S]
  // |-------| <- Finalizer of Stack above { 
  //                                        \ onRewind: S -> Control[Unit]
  //    ...
  // If a stack is unwound, the Finalizer stack also has to be unwound in order to get to the stacks below.
  // This ensures that at each unwind, the unwind operation is always executed. The same applies for rewinding.
  // (frames: List[Frame], clauses: Clauses, onUnwindData: A, tail: Stack) -> Stack
  function Finalizer(clauses, onUnwindData, doRewindOp, tail) {
    return { 
      clauses: clauses, onUnwindData: onUnwindData, isFinilizer: true, doRewindOp: doRewindOp, tail: tail }
  }


  function Clauses(onUnwind, onRewind) {
    return {
      onUnwind: onUnwind, onRewind: onRewind
    }
  }

  const EmptyClauses = Clauses(null, null)

  // return a to stack
  // (stack: Stack<A, B>, a: A) -> Step<B>
  function apply(stack, a) {
    var s = stack;
    while (true) {
      if (s === EmptyStack) return a;
      // a cannot be applied to a finalizer stack
      if (s.isFinilizer) {
        s = s.tail;
        continue
      }
      const fs = s.frames;
      if (fs === Nil) {
        if (s.onReturn != null) {
          // Execute the return operation within the outer context.
          // Since s is not a finalizer, it is safe to assume that
          // s.tail is finalizer, hence it is neccessary to execute
          // the return operation within s.tail.tail, which is in
          // of type Stack again.
          return Step(s.onReturn(a), s.tail.tail)
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
  function pushSubcont(subcont, stack, c) {
    var sub = subcont;
    var s = stack;

    while (sub !== EmptyStack) {
      if (sub.isFinilizer) {
        if (sub.doRewindOp && sub.clauses.onRewind != null) {
          // execute the rewind operation and remember to push the remaining substack
          // onto the resulting metastack k
          const c_ = sub.clauses.onRewind(sub.onUnwindData)
              .then(() => Control(k => {
                // remember that the rewind operation already has been executed and shall
                // not be executed again in the next iteration
                sub = Finalizer(sub.clauses, sub.onUnwindData, false, sub.tail);
                return pushSubcont(sub, k, c)
              }));
              // s is always of type Stack here. 
              // Proof: Assuming s would be of type Finalizer, then the predecesssor of the current 
              // sub had to be a Finalizer. However, since the current sub is the tail of the last, 
              // sub cannot be a Finalizer again due to the invariant that holds for every Finalizer. 
              // That means we would not be wihtin this if branch.
          return Step(c_, s)
        } else {
          s = Finalizer(sub.clauses, sub.onUnwindData, true, s);
        }
      } else {
        s = Stack(sub.frames, restore(sub.backup), sub.prompt, sub.onReturn, s);
      }
      sub = sub.tail;
    }
    return Step(c, s);
  }

  // Pushes a frame f onto the stack
  // (stack: Stack, f: Frame) -> Stack
  function flatMap(stack, f) {
    if (stack === EmptyStack) { return Stack(Cons(f, Nil), [], null, null, stack) }
    var fs = stack.frames
    // it should be safe to mutate the frames field, since they are copied in the subcont
    stack.frames = Cons(f, fs)
    return stack
  }

  // Corresponds to a stack unwind. Pops off stacks until the stack with prompt has been found
  // (stack: Stack, sub: Stack, p: Int, f: Frame) -> Step
  function splitAt(stack, subcont, p, f) {
    var sub = subcont;
    var s = stack;

    while (s != EmptyStack) {
      if (s.isFinilizer) {
        if (s.clauses.onUnwind != null) {
          // execute the unwind operation within the outer context,
          // remember to pop s afterwards and continue poping off until p has been found.
          const c = s.clauses.onUnwind().then(a =>
            Control(k => {
              sub = Finalizer(s.clauses, a, true, sub);
              return splitAt(k, sub, p, f)
            })
          )
          // s.tail is a Stack because of the invariant that holds for every
          // Finalizer
          return Step(c, s.tail)
        } else {
          sub = Finalizer(s.clauses, null, true, sub);
        }
      } else {
        const currentPrompt = s.prompt;
        sub = SubStack(s.frames, backup(s.fields), currentPrompt, s.onReturn, sub);
        if (currentPrompt === p) {
          // if the prompt has been found the unwind operation, as well as the rewind
          // operation of the current stack, are not to be executed.
          sub = Finalizer(s.tail.clauses, null, false, sub);
          const localCont = a => Control(k => pushSubcont(sub, k, pure(a), p, 0));
          // Due to invariant: s.tail.tail is of type Stack
          return Step(f(localCont), s.tail.tail)
        }
      }
      s = s.tail;
    }
    throw ("Prompt " + p + " not found");
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
      run: () => trampoline(Step(self, Stack(Nil, [], toplevel, null, EmptyStack))),
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

  const reset = p => clauses => c => Control(k => {
    // Establish invariant: Stack -> Finalizer -> k
    const fS = Finalizer(Clauses(clauses.onUwind, clauses.onRewind), null, true, k);
    const s = Stack(Nil, [], p, clauses.onReturn, fS);
    return Step(c, s)
  })

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
    return body => reset(p)
                        ({ onUwind: onUnwind, onRewind: onRewind, onReturn: onReturn })
                        (body.apply(null, caps))
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