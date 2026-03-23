// Complexity of state:
//
//  get: O(1)
//  set: O(1)
//  capture: O(1)
//  restore: O(|write operations since capture|)
const Mem = null

// reusable buffer for rerooting
const _rerootPath = []

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

function snapshot(s) {
  const snap = { store: s, root: s.root, generation: s.generation }
  s.generation = s.generation + 1
  return snap
}

function reroot(target) {
  // 1. Walk from target toward the Mem node, collecting the path into `_rerootPath`
  _rerootPath.length = 0
  let cur = target
  while (cur.value !== Mem) {
    _rerootPath.push(cur)
    cur = cur.value.root
  }
  // cur is now Mem (the current root)

  // 2. Walk back from current root toward target,
  //    reverse each edge, restore each ref.
  for (let i = _rerootPath.length - 1; i >= 0; i--) {
    const node   = _rerootPath[i]
    const diff   = node.value // forward diff
    const r      = diff.ref
    cur.value    = { ref: r, value: r.value, generation: r.generation, root: node }
    r.value      = diff.value      // restore ref's old value
    r.generation = diff.generation //  ... and old generation
    node.value   = Mem
    cur          = node
  }
}

function restore(store, snap) {
  if (snap.root.value !== Mem) {
    // fast path: continuation is resumed immediately with no writes in between
    reroot(snap.root)
    store.root = snap.root
  }
  store.generation = snap.generation + 1
}

// Sentinel snapshot used for null arenas — restoring it is always a no-op.
const NULL_SNAP = { root: { value: Mem }, generation: -1 }

// Common Runtime
// --------------
let _prompt = 1;

const TOPLEVEL_K = (x, ks) => { throw { computationIsDone: true, result: x } }
const TOPLEVEL_KS = { prompt: 0, arena: Arena(), rest: null }

function THUNK(f) {
  f.thunk = true
  return f
}

function VAR(init, ks) {
  if (ks.arena === null) ks.arena = Arena()
  return ks.arena.fresh(init)
}

function REGION(ks) {
  if (ks.arena === null) ks.arena = Arena()
  return ks.arena.newRegion()
}

function CAPTURE(body) {
  return (ks, k) => {
    const res = body(x => TRAMPOLINE(() => k(x, ks)))
    if (res instanceof Function) return res
    else throw { computationIsDone: true, result: $effekt.unit }
  }
}

const RETURN = (x, ks) => ks.rest.stack(x, ks.rest)

// HANDLE(ks, ks, (p, ks, k) => { STMT })
function RESET(prog, ks, k) {
  const prompt = _prompt++;
  const rest = { stack: k, prompt: ks.prompt, arena: ks.arena, rest: ks.rest }
  // arena: null; only materialised on first VAR in this handler scope
  return prog(prompt, { stack: null, prompt, arena: null, rest }, RETURN)
}

function SHIFT(p, body, ks, k) {

  // TODO avoid constructing this object
  let meta = { stack: k, prompt: ks.prompt, arena: ks.arena, rest: ks.rest }
  let cont = null

  while (!!meta && meta.prompt !== p) {
    let store = meta.arena
    // If this handler scope never called VAR, its arena is null.
    // We still capture the frame, but use a no-op sentinel snapshot.
    cont = { stack: meta.stack, prompt: meta.prompt, arena: store,
             backup: store !== null ? snapshot(store) : NULL_SNAP,
             rest: cont }
    meta = meta.rest
  }
  if (!meta) { throw `Prompt not found ${p}` }

  // package the prompt itself
  let store = meta.arena
  cont = { stack: meta.stack, prompt: meta.prompt, arena: store,
           backup: store !== null ? snapshot(store) : NULL_SNAP,
           rest: cont }
  meta = meta.rest

  const k1 = meta.stack
  meta.stack = null
  return body(cont, meta, k1)
}

// Rewind stack `cont` back onto `k` :: `ks` and resume with c
function RESUME(cont, c, ks, k) {
  let meta = { stack: k, prompt: ks.prompt, arena: ks.arena, rest: ks.rest }
  let toRewind = cont
  while (!!toRewind) {
    // arena is null when this handler scope never called VAR ~> skip restore entirely
    if (toRewind.arena !== null) {
      restore(toRewind.arena, toRewind.backup)
    }
    meta = { stack: toRewind.stack, prompt: toRewind.prompt, arena: toRewind.arena, rest: meta }
    toRewind = toRewind.rest
  }

  const k1 = meta.stack // TODO instead copy meta here, like elsewhere?
  meta.stack = null
  return () => c(meta, k1)
}

function RUN_TOPLEVEL(comp) {
  try {
    let a = comp(TOPLEVEL_KS, TOPLEVEL_K)
    while (true) { a = a() }
  } catch (e) {
    if (e.computationIsDone) return e.result
    else throw e
  }
}

// trampolines the given computation (like RUN_TOPLEVEL, but doesn't provide continuations)
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

// keeps the current trampoline going and dispatches the given task
function RUN(task) {
  return () => task(TOPLEVEL_KS, TOPLEVEL_K)
}

// aborts the current continuation
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
