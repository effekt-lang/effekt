---
layout: docs
title: IO
permalink: docs/tutorial/io
---

# IO (Input-Output)

For the sake of demonstrating file IO, we want to open a file, take the first `n` number of lines and write the result into another file.

```effekt:hidden
def filesystem[R] { prog: => R / Files }: R / { AsyncIO, Exception[IOError] } = {
  val err = ENOENT()
  try { prog() }
  with Files {
    def readFile(path) =
      if (path == "README.md") { resume("hello, world\nfrom Effekt!") }
      else { do raise[IOError](err, err.message) }
    def writeFile(path, contents) =
      if (path == "head.txt") { resume(()) }
      else { do raise[IOError](err, err.message) }
  }
}
```

```
import io
import io/files
import io/error
import exception

def headOf(srcPath: String, destPath: String, lines: Int): Unit / Files = {
  val contents = do readFile(srcPath)
  val head = contents.split("\n").take(lines).join("")
  do writeFile(destPath, head)
}
```

Since our function uses effect operations of the effect `Files`, we have to handle it.
This can be done using the effect handler `filesystem` given in the standard library.
Note, that IO is done asynchronously by default in Effekt.
That is why the `fileystem` handler expects the effects `AsyncIO` to be handled.
Furthermore, since our path might not exist, we have to also handle the case an exception is thrown.
Luckily, for these effects there are already handlers present in the standard library we can use:

```
def main() = eventloop(box {
  with on[IOError].panic
  with filesystem

  reverseContentOf("README.md", "head.txt")
})
```

## References

- [IO in the standard library](https://github.com/effekt-lang/effekt/tree/master/libraries/common/io)