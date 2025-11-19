// Type Definitions
// ----------------

/**
 * @typedef {Object} MetaContinuation
 * @property {number} prompt - Continuation prompt ID
 * @property {Object} arena - Memory arena for mutable state
 * @property {MetaContinuation|null} rest - Parent continuation stack
 * @property {Continuation} [stack] - Stack continuation (optional)
 */

/**
 * @callback Continuation
 * @param {*} value - Return value
 * @param {MetaContinuation} ks - Metacontinuation
 * @returns {*}
 */

/**
 * @typedef {Object} Reference
 * @property {*} value - Current value
 * @property {number} generation - Version number
 * @property {Object} store - Memory store
 * @property {function(*): void} set - Update the reference
 */

/**
 * @typedef {Object} DiffNode
 * @property {Reference} ref
 * @property {*} value
 * @property {number} generation
 * @property {MemNode} root
 */

/**
 * @typedef {Object} MemNode
 * @property {DiffNode|null} value
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

/**
 * @returns {{root: MemNode, generation: number, fresh: function(*): Reference, newRegion: function(): *}}
 */
function Arena() {
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

            // @ts-ignore - Setting up diff node
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
 * @param {Object} s - Store to snapshot
 * @returns {{store: Object, root: MemNode, generation: number}}
 */
function snapshot(s) {
  const snap = { store: s, root: s.root, generation: s.generation }
  s.generation = s.generation + 1
  return snap
}

/**
 * @param {MemNode} n - Node to reroot
 * @private
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
  n2.value = { ref: r, value: r.value, generation: r.generation, root: n }
  r.value = v
  r.generation = g
}

/**
 * @param {Object} store
 * @param {{store: Object, root: MemNode, generation: number}} snap
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
 * @returns {function(): T}
 */
function THUNK(f) {
  // @ts-ignore - Adding thunk marker property to function
  f.thunk = true
  return f
}

/**
 * @param {function(function(*): *): (*|function(): *)} body
 * @returns {function(MetaContinuation, Continuation): *}
 */
function CAPTURE(body) {
  return (ks, k) => {
    const res = body(x => TRAMPOLINE(() => k(x, ks)));
    if (res instanceof Function) return res;
    else throw { computationIsDone: true, result: $effekt.unit };
  }
}

/**
 * @param {*} x
 * @param {MetaContinuation} ks
 * @returns {*}
 */
const RETURN = (x, ks) => {
  // @ts-ignore - ks.rest guaranteed non-null in RESET context [?]
  return ks.rest.stack(x, ks.rest)
}

/**
 * @param {function(number, MetaContinuation, Continuation): *} prog
 * @param {MetaContinuation} ks
 * @param {Continuation} k
 * @returns {*}
 */
function RESET(prog, ks, k) {
  const prompt = _prompt++;
  /** @type {MetaContinuation} */
  const rest = { stack: k, prompt: ks.prompt, arena: ks.arena, rest: ks.rest }
  return prog(prompt, { prompt, arena: Arena(), rest }, RETURN)
}

/**
 * @param {number} p - prompt ID
 * @param {function(Object, MetaContinuation, Continuation): *} body
 * @param {MetaContinuation} ks
 * @param {Continuation|undefined} k
 * @returns {*}
 */
function SHIFT(p, body, ks, k) {
  // TODO avoid constructing this `meta` object
  /** @type {{stack: Continuation, prompt: number, arena: Object, rest: MetaContinuation|null}} */
  let meta = { stack: /** @type {Continuation} */(k), prompt: ks.prompt, arena: ks.arena, rest: ks.rest }
  /** @type {Object|null} */
  let cont = null

  while (!!meta && meta.prompt !== p) {
    let store = meta.arena
    cont = { stack: meta.stack, prompt: meta.prompt, arena: store, backup: snapshot(store), rest: cont }
    /** @type {any} */
    const nextMeta = meta.rest
    meta = nextMeta
  }
  if (!meta) { throw `Prompt not found ${p}` }

  // Package the prompt itself
  let store = meta.arena
  cont = { stack: meta.stack, prompt: meta.prompt, arena: store, backup: snapshot(store), rest: cont }
  /** @type {any} */
  const nextMeta = meta.rest
  meta = nextMeta

  const k1 = meta.stack
  // @ts-ignore - Setting to null is intentional
  meta.stack = null
  return body(cont, meta, k1)
}

/**
 * @param {Object} cont
 * @param {*} c
 * @param {MetaContinuation} ks
 * @param {Continuation} k
 * @returns {function(): *}
 */
function RESUME(cont, c, ks, k) {
  /** @type {any} */
  let meta = { stack: k, prompt: ks.prompt, arena: ks.arena, rest: ks.rest }
  /** @type {any} */
  let toRewind = cont
  while (!!toRewind) {
    restore(toRewind.arena, toRewind.backup)
    meta = { stack: toRewind.stack, prompt: toRewind.prompt, arena: toRewind.arena, rest: meta }
    toRewind = toRewind.rest
  }

  const k1 = meta.stack; // TODO instead copy `meta` here, like elsewhere?
  // @ts-ignore - Setting to null is intentional
  meta.stack = null
  return () => c(meta, k1)
}

/**
 * @template T
 * @param {function(MetaContinuation, Continuation): (function(): *)} comp
 * @returns {T}
 */
function RUN_TOPLEVEL(comp) {
  try {
    let a = comp(TOPLEVEL_KS, TOPLEVEL_K)
    while (true) { a = a() }
  } catch (e) {
    if (e.computationIsDone) return e.result
    else throw e
  }
}

/**
 * @template T
 * @param {function(): (T|function(): *)} comp
 * @returns {T}
 */
function TRAMPOLINE(comp) {
  let a = comp;
  try {
    while (true) {
      // @ts-ignore - Dynamic trampolining
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
 * @returns {function(): *}
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
$effekt.capture = CAPTURE;


/**
 * Used to call Effekt function arguments in the JS FFI, like in `io::spawn`.
 *
 * Requires an active Effekt runtime (trampoline).
 */
$effekt.run = RUN;

/**
 * Used to call Effekt function arguments in the JS FFI, like in `network::listen`.
 *
 * This function should be used when _no_ Effekt runtime is available. For instance,
 * in callbacks passed to the NodeJS eventloop.
 *
 * If a runtime is available, use `$effekt.run`, instead.
 */
$effekt.runToplevel = RUN_TOPLEVEL;