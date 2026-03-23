---
title: Computations
permalink: tour/computation
---

# Computations

## Values vs. Computations

Following Paul Levy's [Call-By-Push-Value](https://www.cs.bham.ac.uk/~pbl/papers/thesisqmwphd.pdf), Effekt distinguishes between **values** (such as `42`, `true`, or instances of datatypes) and **computations**.

Examples of "computations" in Effekt are:
- blocks (this is what we call functions),
- instances of interfaces (also known as "objects"), and
- regions

Functions (and all other computation types) are _second-class_ in Effekt. To make this difference explicit, we pass values in parentheses (e.g. `f(42)`) and computations in braces (e.g. `f { x => println(x) }`).

```
def myMap[A, B](xs: List[A]) { f: A => B }: List[B] =
  xs match {
    case Nil() => Nil()
    case Cons(x, xs) => Cons(f(x), myMap(xs) { f })
  }
```

The block parameter `f` is enclosed in curly braces and syntactically separated from the value parameters.
While multiple value parameters are separated by commas, multiple block parameters are expressed by enclosing each in curly braces.

```effekt:repl
[1, 2, 3].myMap { x => x * 2 }
```

In the previous example, the block passed to `myMap` did not use any effect operations and the signature of `f` in `myMap` did not mention any as well. What happens if
the argument passed for `f` is effectful?

```
interface Yield[A] {
  def yield(x: A): Unit
}

def example(): List[Int] / Yield[Int] = [1, 2, 3].map { x => do yield(x); x * 2 }
```

The expanded type of `myMap` makes the effect positions explicit:
```effekt:sketch
myMap[A, B](xs: List[A]) { f: A => B / {} }: List[B] / {}
```

The `/ {}` on `myMap` itself means its caller needs to handle no extra effects.
The `/ {}` on `f` might look like it says "f must be pure", but that is **not** the correct reading.
Instead, it means that `f` imposes *no requirements on `myMap` itself*.
Any effects `f` actually uses simply propagate to the *call-site of `myMap`*,
where they are handled in the lexical scope where `f` was written.

This is *contextual effect polymorphism*: `myMap` stays effect-agnostic regardless of what `f` does.
See [Effect Polymorphism](../docs/concepts/effect-polymorphism) for more details.

## Comparison

|   | Values (First-class) | Computations (Second-class) |
|---|:---|---:|
| Term-level | `42`, `"hello"`, `true`, `Cons(1, Nil)`, `box { [A](x: A) => x }` ... | `{ [A](x: A) => x }`, `new Exception`, `region r`, `unbox exc`  |
| | Literals, instances of datatypes and boxed computations | Blocks, objects and regions |
| Type-level | `Int`, `String`, `Bool`, `List[Int]`, `[A](A) => A at {}`...  | `[A](A) => A`, `Exception`, `Region`  |

On the term-level, `box`ing offers a way of transforming a second-class computation into a first-class value.
Conversely, `unbox`ing does the reverse and converts a first-class boxed computation back into a second-class computation.
On the type-level, `at {...}` serves the same purpose as `box`.
Further information can be found in the section about [captures](./captures) and [objects](./objects).

## References

- [Call-By-Push-Value](https://www.cs.bham.ac.uk/~pbl/papers/thesisqmwphd.pdf)
