// const $effekt = {}
// Common Runtime
// --------------
function Cell(init, region) {
  const cell = {
    value: init,
    backup: function() {
      const _backup = cell.value;
      // restore function (has a STRONG reference to `this`)
      return () => { cell.value = _backup; return cell }
    }
  }
  return cell;
}

const global = {
  fresh: Cell
}

function Arena(_region) {
  const region = _region;
  return {
    fresh: function(init) {
      const cell = Cell(init);
      // region keeps track what to backup, but we do not need to backup unreachable cells
      region.push(cell) // new WeakRef(cell))
      return cell;
    },
    region: _region,
    newRegion: function() {
      // a region aggregates weak references
      const nested = Arena([])
      // this doesn't work yet, since Arena.backup doesn't return a thunk
      region.push(nested) //new WeakRef(nested))
      return nested;
    },
    backup: function() {
      const _backup = []
      let nextIndex = 0;
      for (const ref of region) {
        const cell = ref //.deref()
        // only backup live cells
        if (cell) {
          _backup[nextIndex] = cell.backup()
          nextIndex++
        }
      }
      function restore() {
        const region = []
        let nextIndex = 0;
        for (const restoreCell of _backup) {
          region[nextIndex] = restoreCell() // new WeakRef(restoreCell())
          nextIndex++
        }
        return Arena(region)
      }
      return restore;
    }
  }
}


let _prompt = 1;

const TOPLEVEL_K = (x, ks) => { throw { computationIsDone: true, result: x } }
const TOPLEVEL_KS = { prompt: 0, arena: Arena([]), onSuspend: null, onSuspendData: null, onResume: null, rest: null }

function THUNK(f) {
  f.thunk = true
  return f
}

function CAPTURE(body) {
  return (ks, k) => {
    const res = body(x => TRAMPOLINE(() => k(x, ks)))
    if (res instanceof Function) return res
    else throw { computationIsDone: true, result: $effekt.unit }
  }
}

const RETURN = (x, ks) => ks.rest.stack(x, ks.rest)

// const r = ks.arena.newRegion(); body
// const x = r.alloc(init); body

// HANDLE(ks, ks, (p, ks, k) => { STMT })
function RESET(prog, onSuspend, onResume, onReturn, ks, k) {
  const prompt = _prompt++;
  const rest = { stack: k, prompt: ks.prompt, arena: ks.arena, onSuspend: ks.onSuspend, onSuspendData: ks.onSuspendData, onResume: ks.onResume, rest: ks.rest }
  const onRet = onReturn ? (x, ks) => onReturn(x, ks.rest, ks.rest.stack) : RETURN
  return prog(prompt, { prompt, arena: Arena([]), onSuspend: onSuspend, onSuspendData: null, onResume: onResume, rest: rest }, onRet)
}

function DEALLOC(ks) {
  const arena = ks.arena
  if (!!arena) {
    arena.length = arena.length - 1
  }
}

function SHIFT(p, body, ks, k, cont) {

  // TODO avoid constructing this object
  let meta = { stack: k, prompt: ks.prompt, arena: ks.arena, onSuspend: ks.onSuspend, onSuspendData: ks.onSuspendData, onResume: ks.onResume, rest: ks.rest }
  // let cont = null

  while (!!meta && meta.prompt !== p) {
    cont = { stack: meta.stack, prompt: meta.prompt, backup: meta.arena.backup(), onSuspend: meta.onSuspend, onSuspendData: meta.onSuspendData, onResume: meta.onResume, rest: cont }
    if (meta.onSuspend) {
      return meta.onSuspend(meta.rest, (x, ks) => {
        // TODO what about ks here? It is just ignored -- doesn't seem right
        // this will probably become a problem once a resumptive effect operation is called in onSuspend

        // just change onSuspendData to retain the value returned by onSuspend
        const contt = { ...cont, onSuspendData: x }
        return SHIFT(p, body, meta.rest, meta.rest.stack, contt)
      })
    }
    meta = meta.rest
  }
  if (!meta) { throw `Prompt not found ${p}` }

  // package the prompt itself
  cont = { stack: meta.stack, prompt: meta.prompt, backup: meta.arena.backup(), onSuspend: meta.onSuspend, onSuspendData: meta.onSuspendData, onResume: meta.onResume, rest: cont }
  meta = meta.rest

  const k1 = meta.stack
  // TODO why is this needed?
  // meta.stack = null
  return body(cont, meta, k1)
}

// Rewind stack `cont` back onto `k` :: `ks` and resume with c
// TODO I do not like the additional argument b here.
// It is needed because when resuming, that is, rewinding cont onto k :: ks, we would
// execute the on resume clause of the top-most stack frame, however, these are not the desired semantics.
function RESUME(cont, c, b, ks, k) {
  let meta = { stack: k, prompt: ks.prompt, arena: ks.arena, onSuspend: ks.onSuspend, onSuspendData: ks.onSuspendData, onResume: ks.onResume, rest: ks.rest }
  let toRewind = cont
  while (!!toRewind) {
    if (toRewind.onResume && b) {
      return toRewind.onResume(toRewind.onSuspendData, meta, (x, ks) => {
        // push stack back onto the meta-stack before resuming later
        meta = { stack: toRewind.stack, prompt: toRewind.prompt, arena: toRewind.backup(), onSuspend: toRewind.onSuspend, onSuspendData: toRewind.onSuspendData, onResume: toRewind.onResume, rest: ks }
        return RESUME(toRewind.rest, c, true, meta, meta.stack)
      })
    }
    b = true
    meta = { stack: toRewind.stack, prompt: toRewind.prompt, arena: toRewind.backup(), onSuspend: toRewind.onSuspend, onSuspendData: toRewind.onSuspendData, onResume: toRewind.onResume, rest: meta }
    toRewind = toRewind.rest
  }

  const k1 = meta.stack // TODO instead copy meta here, like elsewhere?
  // TODO why is this needed?
  // meta.stack = null
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

/*
function main_94(ks_1463, k_1130) {
  return RESET((p_93, ks_1467, k_1134) => {
    const Eff_3 = {
      Eff_4: (ks_1468, k_1135) =>
        SHIFT(p_93, (k_1136, ks_1469, k_1137) => {
          function resume_84(a_88, ks_1470, k_1138) {
            return RESUME(k_1136, (ks_1471, k_1139) =>
              () => k_1139(a_88, ks_1471), false, ks_1470, k_1138);
          }
          const _285 = console.log("Eff");
          return resume_84($effekt.unit, ks_1469, k_1137);
        }, ks_1468, k_1135)
    };
    return RESET((p_94, ks_1475, k_1143) =>
      Eff_3.Eff_4(ks_1475, (_289, ks_1476) => () => k_1143(42, ks_1476)), (ks_1472, k_1140) => {
      const _286 = console.log("suspending inner");
      return Eff_3.Eff_4(ks_1472, k_1140);
    }, (_287, ks_1473, k_1141) => {
      const v_r_655 = console.log("resuming inner");
      return () => k_1141(v_r_655, ks_1473);
    }, (x_217, ks_1474, k_1142) => {
      const _288 = console.log("returning inner");
      return () => k_1142((x_217 * (2)), ks_1474);
    }, ks_1467, k_1134);
  }, (ks_1464, k_1131) => {
    const v_r_653 = console.log("suspending outer");
    return () => k_1131(v_r_653, ks_1464);
  }, (_283, ks_1465, k_1132) => {
    const v_r_654 = console.log("resuming outer");
    return () => k_1132(v_r_654, ks_1465);
  }, (x_216, ks_1466, k_1133) => {
    const _284 = console.log("returning outer");
    return () => k_1133((x_216 + (3)), ks_1466);
  }, ks_1463, (x_218, ks_1477) => {
    const v_r_656 = console.log(('' + x_218));
    return () => k_1130($effekt.unit, ks_1477);
  });
}

RUN_TOPLEVEL(main_94)
*/
