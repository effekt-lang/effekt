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

// Memory sentinel
const Mem = null;

/**
 * Create a new memory arena for isolated state
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
          const s = r.store;
          const r_gen = r.generation;
          const s_gen = s.generation;

          if (r_gen == s_gen) {
            r.value = v;
          } else {
            const root = { value: Mem };
            // @ts-ignore - Complex persistent data structure
            s.root.value = { ref: r, value: r.value, generation: r_gen, root: root };
            s.root = root;
            r.value = v;
            r.generation = s_gen;
          }
        }
      };
      return r;
    },
    newRegion: () => s
  };
  return s;
}

/**
 * Capture a snapshot of memory state
 * @param {Object} s - Store to snapshot
 * @returns {{store: Object, root: MemNode, generation: number}}
 */
function snapshot(s) {
  const snap = { store: s, root: s.root, generation: s.generation };
  s.generation = s.generation + 1;
  return snap;
}

/**
 * Restore memory to a previous snapshot
 * @param {Object} store - Current store
 * @param {{store: Object, root: MemNode, generation: number}} snap - Snapshot to restore
 * @returns {void}
 */
function restore(store, snap) {
  reroot(snap.root);
  store.root = snap.root;
  store.generation = snap.generation + 1;
}

/**
 * Internal rerooting for persistent state
 * @param {MemNode} n - Node to reroot
 * @private
 */
function reroot(n) {
  if (n.value === Mem) return;

  const diff = n.value;
  const r = diff.ref;
  const v = diff.value;
  const g = diff.generation;
  const n2 = diff.root;
  reroot(n2);
  n.value = Mem;
  n2.value = { ref: r, value: r.value, generation: r.generation, root: n};
  r.value = v;
  r.generation = g;
}

/**
 * Wrap a thunk for lazy evaluation
 * @template T
 * @param {function(): T} f - Thunk function
 * @returns {function(): T}
 */
function THUNK(f) {
  // @ts-ignore - Adding thunk marker property to function
  f.thunk = true;
  return f;
}

/**
 * Capture current continuation
 * @param {function(function(*): *): (*|function(): *)} body - Function receiving continuation
 * @returns {function(MetaContinuation, Continuation): *}
 */
function CAPTURE(body) {
  return (ks, k) => {
    const res = body(x => TRAMPOLINE(() => k(x, ks)));
    if (res instanceof Function) return res;
    else throw { computationIsDone: true, result: $effekt.unit };
  };
}

/**
 * Return from current continuation
 * @param {*} x - Value to return
 * @param {MetaContinuation} ks - Current metacontinuation
 * @returns {*}
 */
const RETURN = (x, ks) => {
  // @ts-ignore - ks.rest guaranteed non-null in RESET context
  return ks.rest.stack(x, ks.rest);
};

/**
 * Delimit a continuation with a prompt
 * @param {function(number, MetaContinuation, Continuation): *} prog - Program
 * @param {MetaContinuation} ks - Current metacontinuation
 * @param {Continuation} k - Current continuation
 * @returns {*}
 */
function RESET(prog, ks, k) {
  const prompt = _prompt++;
  /** @type {MetaContinuation} */
  const rest = { stack: k, prompt: ks.prompt, arena: ks.arena, rest: ks.rest };
  return prog(prompt, { prompt, arena: Arena(), rest }, RETURN);
}

/**
 * Shift control to captured continuation
 * @param {number} p - Prompt to shift to
 * @param {function(Object, MetaContinuation, Continuation): *} body - Handler
 * @param {MetaContinuation} ks - Current metacontinuation
 * @param {Continuation|undefined} k - Current continuation
 * @returns {*}
 */
function SHIFT(p, body, ks, k) {
  /** @type {{stack: Continuation, prompt: number, arena: Object, rest: MetaContinuation|null}} */
  let meta = { stack: /** @type {Continuation} */(k), prompt: ks.prompt, arena: ks.arena, rest: ks.rest };
  /** @type {Object|null} */
  let cont = null;

  while (!!meta && meta.prompt !== p) {
    let store = meta.arena;
    cont = { stack: meta.stack, prompt: meta.prompt, arena: store, backup: snapshot(store), rest: cont };
    /** @type {any} */
    const nextMeta = meta.rest;
    meta = nextMeta;
  }
  if (!meta) { throw `Prompt not found ${p}`; }

  // Package the prompt itself
  let store = meta.arena;
  cont = { stack: meta.stack, prompt: meta.prompt, arena: store, backup: snapshot(store), rest: cont };
  /** @type {any} */
  const nextMeta = meta.rest;
  meta = nextMeta;

  const k1 = meta.stack;
  // @ts-ignore - Setting to null is intentional
  meta.stack = null;
  return body(cont, meta, k1);
}

/**
 * Resume a captured continuation
 * @param {Object} cont - Captured continuation
 * @param {*} c - Value to resume with
 * @param {MetaContinuation} ks - Current metacontinuation
 * @param {Continuation} k - Current continuation
 * @returns {function(): *}
 */
function RESUME(cont, c, ks, k) {
  /** @type {any} */
  let meta = { stack: k, prompt: ks.prompt, arena: ks.arena, rest: ks.rest };
  /** @type {any} */
  let toRewind = cont;
  while (!!toRewind) {
    restore(toRewind.arena, toRewind.backup);
    meta = { stack: toRewind.stack, prompt: toRewind.prompt, arena: toRewind.arena, rest: meta };
    toRewind = toRewind.rest;
  }

  const k1 = meta.stack;
  // @ts-ignore - Setting to null is intentional
  meta.stack = null;
  return () => c(meta, k1);
}

/**
 * Run computation at top level
 * @template T
 * @param {function(MetaContinuation, Continuation): (function(): *)} comp - Computation
 * @returns {T}
 */
function RUN_TOPLEVEL(comp) {
  try {
    let a = comp(TOPLEVEL_KS, TOPLEVEL_K);
    while (true) {
      a = a();
    }
  } catch (e) {
    if (e.computationIsDone) return e.result;
    else throw e;
  }
}

/**
 * Trampoline a computation
 * @template T
 * @param {function(): (T|function(): *)} comp - Computation
 * @returns {T}
 */
function TRAMPOLINE(comp) {
  let a = comp;
  try {
    while (true) {
      // @ts-ignore - Dynamic trampolining
      a = a();
    }
  } catch (e) {
    if (e.computationIsDone) return e.result;
    else throw e;
  }
}

/**
 * Dispatch task on current trampoline
 * @param {function(MetaContinuation, Continuation): *} task - Task
 * @returns {function(): *}
 */
function RUN(task) {
  return () => task(TOPLEVEL_KS, TOPLEVEL_K);
}

/**
 * Abort current continuation with value
 * @param {*} value - Value to abort with
 * @throws {{computationIsDone: boolean, result: *}}
 * @returns {never}
 */
function ABORT(value) {
  throw { computationIsDone: true, result: value };
}

// Public API exports
$effekt.capture = CAPTURE;
$effekt.run = RUN;
$effekt.runToplevel = RUN_TOPLEVEL;

// Internal constants
let _prompt = 1;

/** @type {Continuation} */
const TOPLEVEL_K = (x, ks) => {
  throw { computationIsDone: true, result: x };
};

/** @type {MetaContinuation} */
const TOPLEVEL_KS = { prompt: 0, arena: Arena(), rest: null };