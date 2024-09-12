---
layout: docs
title: Build System
permalink: docs/casestudies/buildsystem
---

# Build System

In this case study, we start reproducing the build systems from:

> "Build Systems à la Carte"
> Andrey Mokhov, Neil Mitchell, Simon Peyton Jones
> ICFP '18

As usual, we start with the module name and imports.

```
module examples/casestudies/buildsystem
```

This example revolves around a single effect: `Need`.

```
type Key = String
type Val = Int

effect need(key: Key): Val
```

The `need` effect operation requests the value for a specific key. In this example keys are strings and values are integers.

A build system defines rules that specify how to build the values for each key. The values for some keys are inputs. For those we have a second effect, `needInput`.

```
effect needInput(key: Key): Val
```

With these two effect operations, we can express the rules for the spreadsheet example from the paper. We define the rules for cells `"B1"` and `"B2"` and treats the other cells as inputs.

Here is the spreadsheet example from "Build systems ala Carte", modeling the
spreadsheet:

|        | A  | B       |
| ------ | -- | ------- |
| **1**  | 10 | A1 + A2 |
| **2**  | 20 | B1 * 2  |

```
def example1(key: Key): Val / { need, needInput } = {
    println(key);
    if (key == "B1") do need("A1") + do need("A2")
    else if (key == "B2") do need("B1") * 2
    else do needInput(key)
}
```

This example explains how to get the value for a given key. It uses the `need` operation when it needs the result for another key and it uses the `needInput` operation when it needs an input.

A build system is a handler for the `need` effect.

```
def build(target: Key) { tasks: Key => Val / { need } }: Val / {} =
    try { tasks(target) }
    with need { requestedKey =>
        resume(build(requestedKey) { k => tasks(k) })
    }
```

This handler function recursively calls itself when a key is needed. It duplicates work for the same key.

Another handler would memoize keys once they are built to avoid duplication.

```
effect keyNotFound(key: Key): Nothing

type Store = List[(Key,Val)]

def find(store: Store, key: Key): Val / keyNotFound = {
    store match {
        case Nil() => do keyNotFound(key)
        case Cons((k, v), xs) => if (k == key) { v } else { find(xs, key) }
    }
}
```

For this we need a store, which we represent as a list of pairs of keys and values.

The `memo` handler function tries to look up the needed key in the store. If the key is found it returns the associated value. Otherwise it itself uses `need`, stores the result, and returns the value.

```
def memo[R] { prog: => R / { need } }: R / { need } = {
    var store: Store = Nil();
    try {
        prog()
    } with need { (key) =>
        try {
            resume(find(store, key))
        } with keyNotFound { (k) =>
            val v = do need(k);
            store = Cons((k, v), store);
            resume(v)
        }
    }
}
```

A second example needs the same key twice.

```
// needing the same key twice
def example2(key: Key) = {
    println(key);
    if (key == "B1") do need("A1") + do need("A2")
    else if (key == "B2") do need("B1") * do need("B1")
    else do needInput(key)
}
```

When we run this example without memoization we will see `"B1"`, `"A1"`, and `"A2"` printed twice. When we add the `memo` handler function we do not as the result is reused.

Finally, to supply the inputs, we have a handler for the `needInput` effect.

```
def supplyInput[R](store: Store) { prog: => R / { needInput } }: R / { keyNotFound } = {
    try { prog() } with needInput { (key) => resume(find(store, key)) }
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
    } with keyNotFound { (key) => println("Key not found: " ++ key) }
}
```
