const $runtime = (function() {


  const pure = a => k => k(a)

  // that's literally the same:
  const then = m => f => k => m(a => f(a)(k))
  const lift = m => k1 => k2 => m(a => k1(a)(k2))

  const shift = body => body
  const id = x => x

  const run = prog => prog(id)
  const reset = prog => prog(pure)

  // { "__tag": "Flip", "__data": [op$Flip], "op$Flip": op$Flip } }
  const liftCap = (ev, c) => {

    // this reflective method is probably not very fast...
    var newCap = {}
    for (var m in c) {
      let f = c[m]
      if (c.hasOwnProperty(m) && (f instanceof Function)) {
        newCap[m] = liftBlock(ev, f)
      }
    }
    return newCap
  }

  const liftBlock = (ev, b) => function() {
    const args = Array.from(arguments)
    return ev(b.apply(null, args))
  }

  // (Unit => t) => Eff t
  const delayed = thunk => k => k(thunk())

  const callcc = f => { throw "callcc not yet supported" }



  function _while(c, body) {
    return bind(c)(b => b ? bind(body)(() => _while(c, body)) : pure(null))
  }

  function handle(handlers) {

    // modify all implementations in the handlers to capture the continuation
    const caps = handlers.map(h => {
      var cap = Object.create({})
      for (var op in h) {
        const impl = h[op];
        cap[op] = function(lift) {
          // the first argument is the lift
          // console.log(arguments)
          const args = Array.from(arguments).slice(1);
          return lift(shift(k => impl.apply(null, args.concat([(ev, arg) => ev(k(arg))]))))
        }
      }
      return cap;
    });
    return body => reset(body.apply(null, [lift].concat(caps)))
  }

  return {
    pure: pure,
    run: run,

    // scopes
    here: id,
    nested: function() {
      const args = arguments
      if (args.length == 2) {
        return m => args[0](args[1](m))
      } else if (args.length == 3) {
        return m => args[0](args[1](args[2](m)))
      } else {
        throw "Only specialized to two lifts, yet"
      }
    },
    callcc: callcc,
    delayed: delayed,
    liftCap: liftCap,
    liftBlock: liftBlock,
    handle: handle,
    then: then,
    _while: _while,
    constructor: (_, tag) => function() {
      return { __tag: tag, __data: Array.from(arguments) }
    },

    hole: function() { throw "Implementation missing" }
  }
})()

Object.assign($effekt, $runtime);
