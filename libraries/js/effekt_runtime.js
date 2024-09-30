
// Common Runtime
// --------------

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
  fresh: function(init) { return Cell(init) },
  backup: function() {},
  restore: function(_) {}
}


let _prompt = 1;

function THUNK(f) {
  f.thunk = true
  return f
}

const RETURN = (x, ks) => ks.rest.stack(x, ks.rest)

// HANDLE(ks, ks, (p, ks, k) => { STMT })
function RESET(prog, ks, k) {
  const prompt = _prompt++;
  const rest = { stack: k, prompt: ks.prompt, rest: ks.rest }
  return prog(prompt, { prompt, rest }, RETURN)
}

function SHIFT(p, body, ks, k) {
  // TODO avoid constructing this object
  let meta = { stack: k, prompt: ks.prompt, rest: ks.rest }
  let cont = null

  while (!!meta && meta.prompt !== p) {
    cont = { stack: meta.stack, prompt: meta.prompt, rest: cont }
    meta = meta.rest
  }
  if (!meta) { throw `Prompt not found ${p}` }

  // package the prompt itself
  cont = { stack: meta.stack, prompt: meta.prompt, rest: cont }
  meta = meta.rest

  function resumption(a, ks, k) {
    let meta = { stack: k, prompt: ks.prompt, rest: ks.rest }
    let toRewind = cont
    while (!!toRewind) {
      meta = { stack: toRewind.stack, prompt: toRewind.prompt, rest: meta }
      toRewind = toRewind.rest
    }

    let k2 = meta.stack // TODO instead copy meta here, like elsewhere?
    meta.stack = null
    return THUNK(() => k2(a, meta))
  }

  let k1 = meta.stack
  meta.stack = null
  return body(resumption, meta, k1)
}
const TOPLEVEL_K = (x, ks) => x
const TOPLEVEL_KS = { prompt: 0, rest: null }

function RUN(comp) {
  let a = comp(TOPLEVEL_KS, TOPLEVEL_K)
  while (a.thunk) { a = a() }
  return a
}