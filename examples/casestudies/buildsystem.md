# Build System

In this case study, we start reproducing the build systems from:

> "Build Systems à la Carte"
> Andrey Mokhov, Neil Mitchell, Simon Peyton Jones
> ICFP '18

As usual, we start with the module name and imports.

```
module examples/casestudies/buildsystem

import immutable/list
import immutable/option
```

This example revolves around a single effect: `Need`.

```
type Key = String
type Val = Int

effect Need(key: Key): Val
```

The `Need` effect operation requests the value for a specific key. In this example keys are strings and values are integers.

A build system defines rules that specify how to build the values for each key. The values for some keys are inputs. For those we have a second effect, `NeedInput`.

```
effect NeedInput(key: Key): Val
```

With these two effect operations, we can express the rules for the spreadsheet example from the paper. We define the rules for cells `"B1"` and `"B2"` and treats the other cells as inputs.

Here is the spreadsheet example from "Build systems ala Carte", modeling the
spreadsheet:

|        | A  | B       |
| ------ | -- | ------- |
| **1**  | 10 | A1 + A2 |
| **2**  | 20 | B1 * 2  |

```
def example1(key: Key): Val / { Need, NeedInput, Console } = {
    println(key);
    key match {
        case "B1" => do Need("A1") + do Need("A2")
        case "B2" => do Need("B1") * 2
        case _ => do NeedInput(key)
    }
}
```

This example explains how to get the value for a given key. It uses the `Need` operation when it needs the result for another key and it uses the `NeedInput` operation when it needs an input.

A build system is a handler for the `Need` effect.

```
def build(target: Key) { tasks: Key => Val / { Need } }: Val / {} =
    try { tasks(target) }
    with Need { requestedKey =>
        resume(build(requestedKey) { k => tasks(k) })
    }
```

This handler function recursively calls itself when a key is needed. It duplicates work for the same key.

Another handler would memoize keys once they are built to avoid duplication.

```
effect KeyNotFound[A](key: Key): A

type Store = List[(Key,Val)]

def find(store: Store, key: Key): Val / KeyNotFound = {
    store match {
        case Nil() => do KeyNotFound(key)
        case Cons((k, v), xs) => if (k == key) { v } else { find(xs, key) }
    }
}
```

For this we need a store, which we represent as a list of pairs of keys and values.

The `memo` handler function tries to look up the needed key in the store. If the key is found it returns the associated value. Otherwise it itself uses `Need`, stores the result, and returns the value.

```
def memo[R] { prog: R / { Need } }: R / { Need } = {
    var store: Store = Nil();
    try {
        prog()
    } with Need { (key) =>
        try {
            resume(find(store, key))
        } with KeyNotFound { (k) =>
            val v = do Need(k);
            store = Cons((k, v), store);
            resume(v)
        }
    }
}
```

A second example needs the same key twice.

```
// Needing the same key twice
def example2(key: Key) = {
    println(key);
    key match {
        case "B1" => do Need("A1") + do Need("A2")
        case "B2" => do Need("B1") * do Need("B1")
        case _ => do NeedInput(key)
    }
}
```

When we run this example without memoization we will see `"B1"`, `"A1"`, and `"A2"` printed twice. When we add the `memo` handler function we do not as the result is reused.

Finally, to supply the inputs, we have a handler for the `NeedInput` effect.

```
def supplyInput[R](store: Store) { prog: R / { NeedInput } }: R / { KeyNotFound } = {
    try { prog() } with NeedInput { (key) => resume(find(store, key)) }
}
```

The `main` function runs all examples.

```
def main() = {
    val inputs = [("A1", 10), ("A2", 20)];
    try {
        val result1 = supplyInput(inputs) { build ("B2") { (key) => example1(key) } };
        println(result1);
        println("");
        val result2 = supplyInput(inputs) { build ("B2") { (key) => example2(key) } };
        println(result2);
        println("");
        val result3 = supplyInput(inputs) { build ("B2") { (key) => memo { example2(key) } } };
        println(result3)
    } with KeyNotFound { (key) => println("Key not found: " ++ key) }
}
```
