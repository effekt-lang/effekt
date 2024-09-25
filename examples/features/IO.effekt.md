---
layout: docs
title: IO
permalink: docs/tutorial/io
---

# IO (Input-Output)

For the sake of demonstrating file IO, we want to open a file, take the first `n` number of lines and write the result into another file.

First, we need some imports:

```effekt:prelude
import io
import io/filesystem
import io/error
import exception
```

```effekt:hide
def filesystem[R] { prog: => R / Files }: R / { Exception[IOError] } = {
  val err = ENOENT()
  var content = ""
  println("hey!")
  try { prog() }
  with Files {
    def readFile(path) =
      if (path == "README.md") { resume("hello, world\nfrom Effekt!") }
      else { do raise[IOError](err, err.message) }
    def writeFile(path, contents) =
      if (path == "head.txt") { content = contents; resume(()) }
      else { do raise[IOError](err, err.message) }
  }
}
```

We want to write a function that reads the first `lines` of a file and writes the result into another file.
For that, we use the `Files` interface and its operations `readFile` and `writeFile`:

```
def headOf(srcPath: String, destPath: String, lines: Int): Unit / Files = {
  val contents = do readFile(srcPath)
  val head = contents.split("\n").take(lines).join("")
  do writeFile(destPath, head)
}
```

Since our function uses effect operations of the effect `Files`, we have to handle it.
This can be done using the effect handler `filesystem` given in the standard library.
Note that IO is done asynchronously by default in Effekt.
Every IO action is executed within a global event-loop and therefore does not require handling by the user.
Furthermore, since our path might not exist, we have to also handle the case where an exception is thrown.
Luckily, for these effects, there are already handlers present in the standard library we can use:

```
def main() = {
  with on[IOError].panic
  with module0::filesystem

  headOf("README.md", "head.txt", 1)
}
```

```effekt:repl
main()
```

## References

- [IO in the standard library](https://github.com/effekt-lang/effekt/tree/master/libraries/common/io)
