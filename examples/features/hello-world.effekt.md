---
layout: docs
title: HelloWorld
permalink: tour/hello-world
---

# Hello, World

Here is the classical "hello, world" program in Effekt.

```
def main() = {
  println("hello, world")
}
```

We define a `main` function that serves as the entry point to the program. Using `println`, we output the string `hello, world`. For running this program, follow the installation instructions and then call the Effekt executable with the respective file:

```effekt:repl
main()
```

You may also run `effekt --help` to explore all the available options, such as choosing a different target backend other than the default JavaScript one.
