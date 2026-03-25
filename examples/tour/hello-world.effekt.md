---
title: HelloWorld
permalink: tour/hello-world
---

# Hello, World

Effekt is a research language built around _lexical effect handlers_: a uniform mechanism that subsumes exceptions, generators, async/await, nondeterminism, and more.

This tour moves quickly and assumes you can already program in some language; links throughout point to deeper explanations.

Here is the classic "hello, world" program in Effekt:

```effekt
def main() = {
  println("hello, world")
}
```

We define a `main` function that serves as the entry point to the program.
Using `println`, we output the string `hello, world`.

To run this program, follow the installation instructions and then call the Effekt executable with the respective file:

```effekt:repl
main()
```

You may also run `effekt --help` to explore all the available options, such as choosing a different target backend other than the default JavaScript one.
