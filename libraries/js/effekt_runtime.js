// Type Definitions
// ----------------

/**
 * @typedef {function(): *} Thunk
 */

/**
 * Prompt ID type for better type on hover
 * @typedef {number} Prompt
 */

/**
 * @typedef {Object} Arena
 * @property {MemNode<*>} root
 * @property {number} generation
 * @property {<T>(t: T) => Reference<T>} fresh
 * @property {function(): Arena} newRegion
 */

/**
 * @typedef {Object} MetaContinuation
 * @property {Prompt} prompt - Continuation prompt ID
 * @property {Arena} arena - Memory arena for mutable state
 * @property {MetaContinuation|null} rest - Parent continuation stack
 * @property {Continuation|null} [stack] - Stack continuation (optional, can be null)
 */

/**
 * @typedef {Object} CapturedContinuation
 * @property {Prompt} prompt - Prompt ID
 * @property {Arena} arena - Memory arena
 * @property {CapturedContinuation|null} rest - Nested captured continuation
 * @property {Continuation|null} [stack] - Stack continuation (optional, can be null)
 * @property {Snapshot} backup - Arena backup snapshot
 */

/**
 * @callback Continuation
 * @param {*} value - Return value
 * @param {MetaContinuation} ks - Metacontinuation
 * @returns {Thunk}
 */

/**
 * Resume function passed to CAPTURE body - call with a value to resume the continuation
 * @callback ResumeFn
 * @param {*} value - Value to pass to the continuation
 * @returns {*}
 */

/**
 * @template T
 * @typedef {Object} Reference
 * @property {T} value - Current value
 * @property {number} generation - Version number
 * @property {Arena} store - Memory store
 * @property {function(T): void} set - Update the reference
 */

/**
 * @template T
 * @typedef {Object} DiffNode
 * @property {Reference<T>} ref
 * @property {T} value
 * @property {number} generation
 * @property {MemNode<T>} root
 */

/**
 * @template T
 * @typedef {Object} MemNode
 * @property {DiffNode<T>|null} value
 */

// State Management
// ----------------

// Complexity of state:
//
//  get: O(1)
//  set: O(1)
//  capture: O(1)
//  restore: O(|write operations since capture|)

// Memory sentinel
const Mem = null;

function Arena() {
  /** @type {Arena} */
  const s = {
    root: { value: Mem },
    generation: 0,
    fresh: (v) => {
      const r = {
        value: v,
        generation: s.generation,
        store: s,
        set: (v) => {
          const s = r.store
          const r_gen = r.generation
          const s_gen = s.generation

          if (r_gen == s_gen) {
            r.value = v;
          } else {
            const root = { value: Mem }
            // update store
            s.root.value = { ref: r, value: r.value, generation: r_gen, root: root }
            s.root = root
            r.value = v
            r.generation = s_gen
          }
        }
      };
      return r
    },
    // not implemented
    newRegion: () => s
  };
  return s
}

/**
 * @typedef {Object} Snapshot
 * @property {Arena} store
 * @property {MemNode<*>} root
 * @property {number} generation
 */

/**
 * @param {Arena} s - Store to snapshot
 */
function snapshot(s) {
  /** @type {Snapshot} */
  const snap = { store: s, root: s.root, generation: s.generation }
  s.generation = s.generation + 1
  return snap
}

/**
 * @template T
 * @param {MemNode<T>} n - Node to reroot
 */
function reroot(n) {
  if (n.value === Mem) return;

  const diff = n.value
  const r = diff.ref
  const v = diff.value
  const g = diff.generation
  const n2 = diff.root
  reroot(n2)
  n.value = Mem
  n2.value = { ref: r, value: r.value, generation: r.generation, root: n}
  r.value = v
  r.generation = g
}

/**
 * @param {Arena} store
 * @param {Snapshot} snap
 * @returns {void}
 */
function restore(store, snap) {
  // linear in the number of modifications...
  reroot(snap.root)
  store.root = snap.root
  store.generation = snap.generation + 1
}

// Common Runtime
// --------------
let _prompt = 1;

/** @type {Continuation} */
const TOPLEVEL_K = (x, ks) => { throw { computationIsDone: true, result: x } }

/** @type {MetaContinuation} */
const TOPLEVEL_KS = { prompt: 0, arena: Arena(), rest: null }

/**
 * @template T
 * @param {function(): T} f
 */
function THUNK(f) {
  // Add thunk marker property - cast to any for property assignment
  /** @type {*} */(f).thunk = true
  return f
}

/**
 * Captures the current continuation and passes it to body.
 * @param {function(ResumeFn): (*|Thunk)} body - Takes a resume function, returns a value or thunk
 * @returns {function(MetaContinuation, Continuation): *}
 */
function CAPTURE(body) {
  return (ks, k) => {
    const res = body(x => TRAMPOLINE(() => k(x, ks)))
    if (res instanceof Function) return res
    else throw { computationIsDone: true, result: $effekt.unit }
  }
}

/**
 * @param {*} x
 * @param {MetaContinuation} ks
 * @returns {Thunk}
 */
const RETURN = (x, ks) => {
  // ks.rest and ks.rest.stack are guaranteed non-null in RESET context
  const rest = /** @type {MetaContinuation & {stack: Continuation}} */(ks.rest)
  return rest.stack(x, rest)
}

/**
 * @param {function(Prompt, MetaContinuation, Continuation): Thunk} prog
 * @param {MetaContinuation} ks
 * @param {Continuation} k
 * @returns {Thunk}
 */
function RESET(prog, ks, k) {
  const prompt = /** @type {Prompt} */(_prompt++);
  /** @type {MetaContinuation} */
  const rest = { stack: k, prompt: ks.prompt, arena: ks.arena, rest: ks.rest }
  return prog(prompt, { prompt, arena: Arena(), rest }, RETURN)
}

/**
 * @template T
 * @param {Prompt} p - prompt ID
 * @param {function(CapturedContinuation, MetaContinuation, Continuation): T} body
 * @param {MetaContinuation} ks
 * @param {Continuation} [k] - can be undefined
 * @returns {T}
 */
function SHIFT(p, body, ks, k) {
  // TODO avoid constructing this `meta` object
  /** @type {MetaContinuation|null} */
  let meta = { stack: k, prompt: ks.prompt, arena: ks.arena, rest: ks.rest }
  /** @type {CapturedContinuation|null} */
  let cont = null

  while (!!meta && meta.prompt !== p) {
    let store = meta.arena
    cont = { stack: meta.stack, prompt: meta.prompt, arena: store, backup: snapshot(store), rest: cont }
    /** @type {MetaContinuation|null} */
    const nextMeta = meta.rest
    meta = nextMeta
  }
  if (!meta) { throw `Prompt not found ${p}` }

  // package the prompt itself
  let store = meta.arena
  cont = { stack: meta.stack, prompt: meta.prompt, arena: store, backup: snapshot(store), rest: cont }
  // meta.rest is non-null because RESET creates prompts with non-null rest
  const parentMeta = /** @type {MetaContinuation} */(meta.rest)

  const k1 = /** @type {Continuation} */(parentMeta.stack)
  // Setting stack to null (it's been captured in cont)
  parentMeta.stack = null
  return body(/** @type {CapturedContinuation} */(cont), parentMeta, k1)
}

/**
 * @param {CapturedContinuation} cont
 * @param {function(MetaContinuation, Continuation): *} c
 * @param {MetaContinuation} ks
 * @param {Continuation} k
 * @returns {Thunk}
 */
function RESUME(cont, c, ks, k) {
  /** @type {MetaContinuation} */
  let meta = { stack: k, prompt: ks.prompt, arena: ks.arena, rest: ks.rest }
  /** @type {CapturedContinuation|null} */
  let toRewind = cont
  while (!!toRewind) {
    restore(toRewind.arena, toRewind.backup)
    meta = { stack: toRewind.stack, prompt: toRewind.prompt, arena: toRewind.arena, rest: meta }
    toRewind = toRewind.rest
  }

  const k1 = /** @type {Continuation} */(meta.stack)
  // Setting stack to null (it's been captured/restored)
  meta.stack = null
  return () => c(meta, k1)
}

/**
 * @template T
 * @param {function(MetaContinuation, Continuation): *} comp
 * @returns {T}
 */
function RUN_TOPLEVEL(comp) {
  try {
    let a = comp(TOPLEVEL_KS, TOPLEVEL_K)
    while (true) {
      a = a()
    }
  } catch (e) {
    if (e.computationIsDone) return e.result
    else throw e
  }
}

/**
 * @template T
 * @param {Thunk} comp
 * @returns {T}
 */
function TRAMPOLINE(comp) {
  let a = comp;
  try {
    while (true) {
      a = a()
    }
  } catch (e) {
    if (e.computationIsDone) return e.result
    else throw e
  }
}

/**
 * Keep the current trampoline going and dispatch task on current trampoline
 * @param {function(MetaContinuation, Continuation): *} task
 * @returns {Thunk}
 */
function RUN(task) {
  return () => task(TOPLEVEL_KS, TOPLEVEL_K)
}

/**
 * Abort current continuation with value
 * @param {*} value - Value to abort with
 * @throws {{computationIsDone: boolean, result: *}}
 * @returns {never}
 */
function ABORT(value) {
  throw { computationIsDone: true, result: value }
}


// "Public API" used in FFI
// ------------------------

/**
 * Captures the current continuation as a WHOLE and makes it available
 * as argument to the passed body. For example:
 *
 *   $effekt.capture(k => ... k(42) ...)
 *
 * The body
 *
 *   $effekt.capture(k => >>> ... <<<)
 *
 * conceptually runs on the _native_ JS call stack. You can call JS functions,
 * like `setTimeout` etc., but you are not expected or required to return an
 * Effekt value like `$effekt.unit`. If you want to run an Effekt computation
 * like in `io::spawn`, you can use `$effekt.run`.
 *
 * Advanced usage details:
 *
 * The value returned by the function passed to `capture` returns
 * - a function: the returned function will be passed to the
 *   Effekt runtime, which performs trampolining.
 *   In this case, the Effekt runtime will keep running, though the
 *   continuation has been removed.
 *
 * - another value (like `undefined`): the Effekt runtime will terminate.
 */
$effekt.capture = CAPTURE

/**
 * Used to call Effekt function arguments in the JS FFI, like in `io::spawn`.
 *
 * Requires an active Effekt runtime (trampoline).
 */
$effekt.run = RUN

/**
 * Used to call Effekt function arguments in the JS FFI, like in `network::listen`.
 *
 * This function should be used when _no_ Effekt runtime is available. For instance,
 * in callbacks passed to the NodeJS eventloop.
 *
 * If a runtime is available, use `$effekt.run`, instead.
 */
$effekt.runToplevel = RUN_TOPLEVEL