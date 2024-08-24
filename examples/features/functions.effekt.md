# Functions

For defining a function, you may use similar syntax know from other languages such as Scala:

```
def fib(n: Int): Int =
  if (n == 0 || n == 1) 1
  else fib(n - 1) + fib(n - 2)
```

Since `fib` is a recursive function, you need to explicitly annotate it. Value parameters like `n` always need to be 
annotated with their type.

Calling functions works as you might expect:

```effekt:repl
fib(5)
```

Perhaps unusual, you can also call `fib` using Effekt's implementation of [Uniform Function Call Syntax
](https://en.wikipedia.org/wiki/Uniform_Function_Call_Syntax#cite_note-4):

```effekt:repl
5.fib()
```

For defining polymorphic (generic) function, like the identity, you need to introduce a type parameter enclosed in square brackets:

```
def identity[A](x: A): A = x
val a = identity(42)
val b = identity[Int](42)
```

When calling the function, you may explicitly pass a type argument for each type parameter or let type inference do its job.

There are also nested function definitions. Thus, we also could have defined `fib` like this:

```
def fib(n: Int): Int = {
  def inner(last: Int, current: Int, count: Int): Int = {
    if (count == 1) current
    else inner(current, last + current)
  }
  inner(0, 1, n)
}
```

```effekt:repl
fib(5)
```