---
layout: docs
title: IO
permalink: docs/tutorial/io
---

# Asynchronous IO (Input-Output)

```effekt:hide
import io
import io/error

def readFile(path: String): String / Exception[IOError] =
  if (internal::existsFile(path)) internal::readFile(path)
  else do raise[IOError](ENOENT(), message(ENOENT()))

def writeFile(path: String, contents: String): Unit / Exception[IOError] =
  internal::writeFile(path, contents)

namespace internal {

  extern js """
    const filesystem = {
      "test1.txt": "Hello world!",
      "test2.txt": "Oh, how wonderful IO is!",
      "test3.txt": "Another file, another content.",
      "test4.txt": "What's gonna be in this file?"
    };

    function delayed(k) {
      const delayInMs = Math.random() * 250;
      setTimeout(k, delayInMs);
    }
  """

  extern def existsFile(path: String): Bool = js "!!filesystem[${path}]"

  extern async def readFile(path: String): String =
    js "$effekt.callcc(k => delayed(() => k(filesystem[${path}])))"

  extern async def writeFile(path: String, contents: String): Unit =
    js "$effekt.callcc(k => delayed(() => k(filesystem[${path}] = ${contents})))"
}

```
Usually, filesystem operations in the Effekt standard library are only available when using the Node.js backend.
For the sake of this tour, we stub the filesystem in the browser and prefill it with the following files:

```text
.
├── test1.txt
├── test2.txt
├── test3.txt
└── test4.txt
```

Effekt establishes the principle of _direct-style IO first_.

This means, if we want to open a file, we do not have to choose between a callback-based variant, a promise-based one, or a direct-style one. All of them are derivable from a single operation -- in direct style:

```effekt:prelude:hide
import io/error
```
```effekt:repl
on[IOError].panic { readFile("test1.txt") }
```

Since `readFile` might fail, we need to handle the `IOError` exception using the panicking handler defined in the standard library.

Let's now write a simple function that swaps the contents of two files.

```
def swap(file1: String, file2: String) = {
  with on[IOError].panic;
  val contents1 = readFile(file1)
  val contents2 = readFile(file2)
  writeFile(file2, contents1)
  writeFile(file1, contents2)
}
```
This implementation is fully direct style and synchronous. All IO operations will be performed sequentially, one-after-another.
```effekt:repl
swap("test1.txt", "test2.txt")
```

## Swapping twice concurrently
If we want to swap two pairs of files, we can do this sequentially, as well.
```
def swapSequential() =  {
  swap("test1.txt", "test2.txt")
  swap("test3.txt", "test4.txt")
}
```
We can also reuse the existing implementation of `swap` and run the two swaps concurrently:
```
def swapConcurrently() = {
  val p1 = promise(box { swap("test1.txt", "test2.txt") })
  val p2 = promise(box { swap("test3.txt", "test4.txt") })
  p1.await;
  p2.await
}
```
```effekt:repl
swapConcurrently()
```
So, while the individual file operations in each call to `swap` are sequential, the two
`swap`s will be interleaved.

## Fully concurrent swap
Finally, we can write a concurrent version of `swap` that performs both reads and writes concurrently:
```
def concurrentSwap(file1: String, file2: String) = {
  val p1 = promise(box {
    with on[IOError].panic;
    val res = readFile(file1);
    println("Done reading " ++ file1);
    res
  })
  val p2 = promise(box {
    with on[IOError].panic;
    val res = readFile(file2);
    println("Done reading " ++ file2);
    res
  })
  val contents1 = p1.await
  val contents2 = p2.await

  val p3 = promise(box {
    with on[IOError].panic;
    writeFile(file2, contents1);
    println("Done writing " ++ file2)
  })

  val p4 = promise(box {
    with on[IOError].panic;
    writeFile(file1, contents2);
    println("Done writing " ++ file1)
  })

  p3.await;
  p4.await
}
```
```effekt:repl
concurrentSwap("test1.txt", "test2.txt")
```

## References

- [IO in the standard library](https://github.com/effekt-lang/effekt/tree/master/libraries/common/io)
