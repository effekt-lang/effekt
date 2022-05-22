const $runtime = (function() {


  const pure = a => k => k(a)

  // that's literally the same:
  const then = m => f => k => m(a => f(a)(k))
  const lift = m => k1 => k2 => m(a => k1(a)(k2))

  const shift = body => body
  const id = x => x

  const run = prog => prog(id)
  const reset = prog => prog(pure)
  const liftBlock = (ev, b) => ev2 => b(m => ev2(ev(m)))

  // TODO delayed(() => console.log("foo"))(pure) is forced
  // (Unit => t) => Eff t
  const delayed = thunk => k => k(thunk())

  const callcc = f => { throw "callcc not yet supported" }

  function _while(c, body) {
    return then(c)(b => b ? then(body)(() => _while(c, body)) : pure($effekt.unit))
  }

  // TODO an alternative strategy for state is to operate on mutable state and save/restore in
  // stateLift.

  function oldState() {

    // the lift that state introduces should lift over the cont AND the reader.
    const state = init => m => m(a => s => k => k(a))(init)
    const stateGet = k => s => k(s)(s)
    const statePut = s => k => s2 => k($effekt.unit)(s)

    // Eff[T, K :: rs] => Eff[T, K :: S :: K :: rs]
    const stateLift = m => k1 => s => k2 => m(a => k1(a)(s)(k2))


    function handleStateOld(init) {
      return body => then(init)(s => {
        return state(s)(body(stateLift)(ev => ({
          "op$get": () => ev(stateGet),
          "op$put": (s) => ev(statePut(s))
        })))
      });
    }
  }

  // This is an alternative form of state using reference cells and
  // lift to backup and restore
  function handleState(init) {
    return body => then(init)(s => {
      var cell = s;
      // is calling the evidence ev here necessary?
      const cap = ev => ({
        "op$get": () => k => k(cell),
        "op$put": (s) => k => { cell = s; return k($effekt.unit) }
      })
      const lift = m => {
        return k => {
          // is this the correct position?
          // the "finalizer"
          const backup = cell
          return m(a => {
            // the "initializer"
            cell = backup
            return k(a)
          })
        }
      }
      return body(lift)(cap)
    })
  }

  function handle(handlers) {

    // modify all implementations in the handlers to capture the continuation
    const caps = handlers.map(h => {
      // capabilities are functions from evidence to handler-object
      return ev => {
        var cap = Object.create({})
        for (var op in h) {
          const impl = h[op];
          // (args..., resume) => ...
          cap[op] = function () {
            const args = Array.from(arguments)
            return ev(shift(k => impl.apply(null, args.concat([ev => arg => ev(k(arg))]))))
          }
        }
        return cap;
      }
    });
    return body => reset(body(lift).apply(null, caps))
  }

  return {
    pure: pure,
    run: run,

    // scopes
    here: id,
    nested: function() {
      const args = arguments
      if (args.length == 2) {
        return m => args[1](args[0](m))
      } else if (args.length == 3) {
        return m => args[2](args[1](args[0](m)))
      } else {
        throw "Only specialized to three lifts, yet"
      }
    },
    callcc: callcc,
    delayed: delayed,
    liftBlock: liftBlock,
    handle: handle,
    state: handleState,
    then: then,
    _while: _while,
    constructor: (_, tag) => function() {
      return { __tag: tag, __data: Array.from(arguments) }
    },

    hole: function() { throw "Implementation missing" }
  }
})()

Object.assign($effekt, $runtime);
