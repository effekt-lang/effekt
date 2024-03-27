(function() {

  // Common Runtime
  // --------------

  // Regions

  function Arena() {
    return {
      fields: [], // Array[Cell],
      fresh: function(init) {
        const cell = Cell(init)
        this.fields.push(cell)
        return cell;
      },
      backup: function() {
        return this.fields.map(c => c.backup())
      },
      restore: function(backup) {
        this.fields = backup.map(c => c());
        return this
      }
    }
  }

  const global = {
    fresh: function(init) { return Cell(init) },
    backup: function() {},
    restore: function(_) {}
  }

  // Lists / Pairs
  function Cons(head, tail) {
    return { head: head, tail: tail }
  }
  const Nil = null

  // reverseOnto[A, B, C](init: Frames[A, B], tail: Stack[B, C]): Stack[A, C]
  function reverseOnto(init, tail) {
    let rest = init;
    let result = tail;
    while (rest !== Nil) {
      result = Cons(rest.head, result)
      rest = rest.tail
    }
    return result
  }

  let _prompt = 2;

  // Monadic Style Runtime
  // ---------------------

  const toplevel = 1;

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

  // Frame = A => Control[B]

  // Metacontinuations / Stacks
  // (frames: List<Frame>, fields: [Cell], prompt: Int, tail: Stack) -> Stack
  function Stack(frames, arena, prompt, tail) {
    return { frames: frames, arena: arena, prompt: prompt, tail: tail }
  }
  function SubStack(frames, arena, backup, prompt, tail) {
    return { frames: frames, arena: arena, backup: backup, prompt: prompt, tail: tail }
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

  // (subcont: Stack, stack: Stack) -> Stack
  function pushSubcont(subcont, stack) {
    var sub = subcont;
    var s = stack;

    while (sub !== EmptyStack) {
      s = Stack(sub.frames, sub.arena.restore(sub.backup), sub.prompt, s)
      sub = sub.tail
    }
    return s;
  }

  function flatMap(stack, f) {
    if (stack === EmptyStack) { return Stack(Cons(f, Nil), Arena(), null, stack) }
    var fs = stack.frames
    // it should be safe to mutate the frames field, since they are copied in the subcont
    stack.frames = Cons(f, fs)
    return stack
  }

  function splitAt(stack, p) {
    var sub = EmptyStack;
    var s = stack;

    while (s !== EmptyStack) {
      const currentPrompt = s.prompt;
      sub = SubStack(s.frames, s.arena, s.arena.backup(), currentPrompt, sub);
      s = s.tail;
      if (currentPrompt === p) { return Cons(sub, s) }
    }
    throw ("Prompt " + p + " not found")
  }

  function allocateInto(stack, p, cell) {
    var s = stack;

    while (s !== EmptyStack) {
      const currentPrompt = s.prompt
      if (currentPrompt === p) {
        return s.fields.push(cell);
      } else {
        s = s.tail
      }
    }
    throw ("Prompt " + p + " not found")
  }

  function withState(init, f) {
    return Control(k => {
      const cell = k.arena.fresh(init)
      return Step(f(cell), k)
    })
  }

  function withRegion(prog) {
    return Control(k => {
      return Step(prog(k.arena), k)
    })
  }

  // Delimited Control
  function Control(apply) {
    const self = {
      apply: apply,
      run: () => trampoline(Step(self, Stack(Nil, global, toplevel, EmptyStack))),
      then: f => Control(k => Step(self, flatMap(k, f))),
      state: f => self.then(init => withState(init, f))
    }
    return self
  }

  const pure = a => Control(k => apply(k, a))

  const delayed = a => Control(k => apply(k, a()))

  const shift = (p, f) => Control(k => {
    const split = splitAt(k, p)
    const localCont = a => Control(k =>
      Step(pure(a), pushSubcont(split.head, k)))
    return Step(f(localCont), split.tail)
  })

  const callcc = f => Control(k => {
    return f(a => trampoline(apply(k, a)))
  })


  function handleMonadic(body) {
    const p = _prompt++;
    return Control(k => Step(body(p), Stack(Nil, Arena(), p, k)))
  }

  // Direct Style Runtime
  // --------------------

  // sealed trait Resumption[A, R]
  // case class Empty[A]() extends Resumption[A, A]
  // case class Segment[A, B, C](head: Stack[B, C], prompt: Prompt, tail: Resumption[A, B]) extends Resumption[A, C]
  class Segment {
    constructor(frames, prompt, region, backup, tail) {
      this.frames = frames;
      this.prompt = prompt;
      this.region = region;
      this.backup = backup;
      this.tail = tail;
    }
  }
  const Empty = null;

  // TODO maybe inline later to save native frames
  function handleOrRethrow(prompt, s, rest) {
    if (!(s instanceof Suspension)) throw s;

    const region = currentRegion
    const k = new Segment(reverseOnto(s.frames, rest), prompt, region, region.backup(), s.cont)
    if (s.prompt === prompt)  {
      return s.body((value) => rewind(k, () => value))
    } else {
      throw new Suspension(s.prompt, s.body, Nil, k)
    }
  }

  function rewind(k, thunk) {
    if (k === Empty) {
      return thunk()
    } else {
      const prompt = k.prompt;
      const region = k.region;
      let rest = k.frames // the pure frames

      // The trampoline
      try {
        enterRegion(region);
        region.restore(k.backup);

        let curr = rewind(k.tail, thunk)
        while (rest !== Nil) {
          const f = rest.head
          rest = rest.tail
          curr = f(curr)
        }
        return curr
      } catch (s) {
        return handleOrRethrow(prompt, s, rest)
      } finally {
        leaveRegion()
      }
    }
  }


  // case class Suspend[A, X, Y, R](
  //   body: Continuation[A, R] => R,
  //   prompt: Prompt,
  //   pure: Frames[X, Y],
  //   cont: Resumption[A, X]
  // )
  class Suspension {
    constructor(p, body, frames, cont) {
      this.prompt = p;
      this.body = body;
      this.frames = frames;
      this.cont = cont;
    }
  }

  // initially the toplevel region.
  let currentRegion = global

  // a stack of regions
  let regions = []

  function enterRegion(r) {
    regions.push(currentRegion)
    currentRegion = r
    return r
  }

  function leaveRegion() {
    const leftRegion = currentRegion
    currentRegion = regions.pop()
    return leftRegion
  }


  // Common API
  // -----------
  $effekt.constructor = (_, tag) => function() {
    return { __tag: tag, __data: Array.from(arguments) }
  }

  $effekt.hole = function() { throw "Implementation missing" }

  $effekt.unit = { __unit: true }

  // Monadic API
  // -----------
  $effekt.pure = pure
  $effekt.callcc = callcc
  $effekt.delayed = delayed
  $effekt.handleMonadic = handleMonadic
  $effekt.ref = Cell
  $effekt.state = withState
  $effekt.shift = shift
  $effekt._if = (c, thn, els) => c ? thn() : els()
  $effekt.withRegion = withRegion

  // Direct style API
  // ----------------
  $effekt.fresh = function(init) {
    return currentRegion.fresh(init)
  }
  $effekt.freshPrompt = function() { return ++_prompt; }
  $effekt.suspend = function(prompt, body) {
    throw new Suspension(prompt, body, Nil, Empty)
  }
  $effekt.suspend_bidirectional = function(prompt, caps, body) {
    throw new Suspension(prompt, body, Cons(thunk => thunk.apply(null, caps), Nil), Empty)
  }

    // suspension: the raised exception.
  $effekt.push = function(suspension, frame) {
    if (!(suspension instanceof Suspension)) throw suspension;
    // Assuming `suspension` is a value or variable you want to return
    throw new Suspension(suspension.prompt, suspension.body,
      Cons(frame, suspension.frames), suspension.cont);
  }
  $effekt.handle = function(prompt, s) {
    return handleOrRethrow(prompt, s, Nil)
  }
  $effekt.freshRegion = function() {
    return enterRegion(Arena())
  }
  $effekt.leaveRegion = leaveRegion
  $effekt.global = global
})()
