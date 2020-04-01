---
layout: page
title:  "Design Considerations"
section: "design-considerations"
position: 2
---


## Key Design Decisions

Compared to other languages with effect handlers (and support for polymorphic effects) the Effekt language
aims to be more lightweight in its concepts.

### No First-Class Functions
For this reason we purposefully
leave out one otherwise very prominent feature: there are no **first-class functions**!

Instead, we treat _all_ functions as _second-class_ ([Osvald et al.](https://www.cs.purdue.edu/homes/rompf/papers/osvald-oopsla16.pdf)).
While functions which (following Ruby jargon) we call _blocks_ can take other
blocks as arguments, they always have to be fully applied.

The following example shows the standard map function on lists:

```effekt
def map[A, B](l: List[A]) { f: A => B } : List[B] =
  l match {
    case Nil() => Nil()
    case Cons(a, rest) => Cons(f(a), map(rest) { a => f(a) })
  }
```
Map takes a value argument `l` and a block argument `f` enclosed in curly braces.
In the case for `Cons` we call `map` on the rest of the list, passing `f`. Note,
how we require `f` to be fully applied. That is, we need to pass a block
that applies `f` (eta-expansion).

The requirement that blocks always have to be (fully) applied and that we do not
have first-class functions has the following implications:

- blocks cannot be returned or stored in variables (they are no values)
- blocks cannot escape their definition site
- blocks do not need to be represented by closures -- all values used in blocks are still available on the call-stack

Maybe most importantly, [effect checking](docs/concepts/effect-system) becomes much easier,
while supporting many advanced use cases.


### Static Effect Checking
Unlike Java (with runtime exceptions) or Scala, in Effekt all effects are fully
tracked by the type system (that is, effect system). For instance using
`println` has an associated effect `Console`.

```effekt
def sayHello(): Unit / { Console } =
  println("Hello World!")
```
While the left component (that is, `Unit`) is the type of values returned by
`sayHello`, the right component of the return type (that is, `{ Console }`)
describes the _set_ of effects required by `sayHello`. That is, it can
only be run in contexts that allow the `Console` effect.

For instance, we cannot call it in a pure function:
```effekt
def pureFun(): Unit / {} = sayHello()
//                         ^^^^^^^^^^
//                 Unhandled effects { Console }
```

In the docs, you find more on the [effect system](docs/concepts/effect-safety).

### Type Annotations on Function Definitions
Like other languages, we require that the parameter types on functions are
fully annotated. Return types (for non-recursive functions) can be inferred,
though. This way, we force the author of a higher-order function definition to
be explicit about the effects of the block arguments.

For instance, in the fully expanded type signature of `map`

```effekt
def map[A, B](l: List[A]) { f: A => B / {} } : List[B] / {}
```

we express that the implementation of `map` cannot handle any effects in `f`
since from the perspective of `map` block argument `f` is assumed to be pure.

More about this can be found in the docs about [effect polymorphism](docs/concepts/effect-polymorphism).
