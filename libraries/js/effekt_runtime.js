const $runtime = (function() {

  // Regions
  // TODO maybe use weak refs (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WeakRef)
  function Cell(init) {
    var _value = init;
    const cell = ({
      backup: function() {
        var _backup = _value
        var cell = this;
        return () => { _value = _backup; return cell }
      }
    });
    // $getOp and $putOp are auto generated from the compiler
    cell[$getOp] = function() {
      return _value
    };
    cell[$putOp] = function(v) {
      _value = v;
      return $effekt.unit;
    };
    return cell
  }

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
    fresh: function(init) { return Cell(init) }
  }

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

  var _prompt = 2;


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
    // trampoline if tailcall
    if (s instanceof TailCall) {
      try { return trampolineTailCall(s) }
      catch (s) { return handleOrRethrow(prompt, s, rest) }

    // attempt handling
    } else if (s instanceof Suspension) {
      const region = currentRegion
      const k = new Segment(reverseOnto(s.frames, rest), prompt, region, region.backup(), s.cont)
      if (s.prompt === prompt)  {
        return s.body((value) => rewind(k, value))
      } else {
        throw new Suspension(s.prompt, s.body, Nil, k)
      }

    // rethrow
    } else { throw s; }
  }

  function rewind(k, value) {
    if (k === Empty) {
      return value
    } else {
      const prompt = k.prompt;
      const region = k.region;
      let rest = k.frames // the pure frames

      // The trampoline
      try {
        enterRegion(region);
        region.restore(k.backup);

        let curr = rewind(k.tail, value)
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

  class TailCall {
    constructor(closure) {
      this.force = closure
    }
  }

  function trampolineTailCall(t) {
    if (!(t instanceof TailCall)) throw t;
    let thunk = t

    while (true) {
      try { return thunk.force() } catch (t) {
        if (!(t instanceof TailCall)) throw t
        thunk = t
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

  return {
    fresh: function(init) {
      return currentRegion.fresh(init)
    },

    constructor: (_, tag) => function() {
      return { __tag: tag, __data: Array.from(arguments) }
    },

    hole: function() { throw "Implementation missing" },

    freshPrompt: function() { return ++_prompt; },

    suspend: function(prompt, body) {
      throw new Suspension(prompt, body, Nil, Empty)
    },
    suspend_bidirectional: function(prompt, caps, body) {
      throw new Suspension(prompt, body, Cons(thunk => thunk.apply(null, caps), Nil), Empty)
    },

    // suspension: the raised exception.
    push: function(suspension, frame) {
      if (suspension instanceof TailCall) {
        try { return trampolineTailCall(suspension) }
        catch (s) { return $effekt.push(s, frame) }
      } else if (suspension instanceof Suspension) {
        // Assuming `suspension` is a value or variable you want to return
        return new Suspension(suspension.prompt, suspension.body,
          Cons(frame, suspension.frames), suspension.cont);
      } else {
        throw suspension
      }
    },

    handle: function(prompt, s) {
      return handleOrRethrow(prompt, s, Nil)
    },

    tailcall: function(thunk) {
      throw new Thunk(thunk)
    },

    run: function(thunk) {
      try { return thunk() } catch (t) { return trampolineTailCall(t) }
    },

    freshRegion: function() {
      return enterRegion(new Arena)
    },

    leaveRegion: leaveRegion,

    global: global
  }
})()

Object.assign($effekt, $runtime);
