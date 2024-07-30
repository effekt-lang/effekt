# Functions

For defining a function, you may use similar syntax for know from other languages such as Scala:

```
def fib(n: Int): Int =
  if (n == 0 || n == 1) 1
  else fib(n - 1) + fib(n - 2)
```

Since `fib` is a recursive function, you need to explicitly annotate it. Value parameters like `n` always need to be 
annotated with their type. Recursive functions like `fib` are stack-safe in Effekt, meaning that a stack-overflow 
cannot occur and are often preferred over while-loops.

Calling functions works as you might expect:

```effekt:repl
fib(5)
```

For defining polymorphic (generic) function, like the identity, you need to introduce a type parameter as known from 
other languages.

```
def identity[A](x: A): A = x
val a = identity(42)
val b = identity[Int](42)
```

When calling the function, you may explicitly pass a type argument for each type parameter or let type inference do its job.

It is important to note that functions are not first-class citizens in Effekt, that is, they are not treated like 
values. This means, among others, that you may not simply return them, save them in collections or use them as function 
arguments. The following thereby is not permitted.

```
// val f = fib
// val fns = [fib, fib]
```

When we talk about boxing, we will see a way to alleviate these potential pain points.
