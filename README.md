# Effekt

Compared to other languages with effect handlers (and support for polymorphic effects) the Effekt language
aims to be significantly more lightweight in its concepts.

## Key Design Decisions

- no first class functions
- instead, we have parametrized (second-class blocks) that always have to be fully applied -- similar to blocks in Ruby
- blocks are first-order, they don't take other blocks
- function definitions have to be annotated with type signatures on the arguments (no ML style inference or let polymorphism)

The requirement that blocks always have to be (fully) applied and that we do not have first-class functions has the following implications:

- blocks only flow up the stack (they cannot be returned or escape otherwise)
- they are thus evaluated in a context / region tighter or equal to their definition context
- blocks do not need to be represented by closures -- all values used in blocks are still available on the call-stack

Maybe most importantly, effect checking becomes almost trivial, while supporting many use cases of effect polymorphism.


## Possible Language Extensions
There are many possible language extensions to grow Effekt into a GPL. When considering extensions, 
we need to check against our top one priority: simplicity. 
Are they really necessary? Do they work with the second class notion of blocks?

- ~val binding (without polymorphism!) -- should be easy~ (implemented)
- ~mutable variables, local control flow (branching, loops) -- should be easy~ (implemented)
- ~algebraic data types, pattern matching -- should be easy as long as constructors can only store values, no blocks~ (draft implemented)
- ~local function definitions -- while requiring FULL annotation of arguments. local functions can "close" over arguments / blocks. This is fine, since they are themselves not first class.~ (implemented)
- multi arity returns
- ~built in "show" and "equality"~ (implemented)
- type classes? Ad hoc polymorphism? overloading?

## Usage

### Compiling

To compile Effekt sources to JavaScript, provide the `--compile` flag.
This will generate JavaScript files in `./out`.

The generated files currently use the `amdefine` module format.
To execute them you need to install `amdefine` and run it with nodeJS:

```
cd out
npm install amdefine
node
> require("./MY_FILE.js").main().run()
```