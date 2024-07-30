# Blocks

```effekt:hidden
interface Yield[A] {
  def yield(x: A): Unit
}
```

As we have established in a previous section, functions in Effekt are second-class. While you cannot pass them as first-class values to other functions as argument, you can pass them as second-class blocks.

```
def map[A, B](xs: List[A]) { f: A => B }: List[B] =
  xs match {
    case Nil() => Nil()
    case Cons(x, xs) => Cons(f(x), map(xs) { f })
  }
```

The parameter `f` is enclosed by curcly braces and is syntactically separated from the value paramters. What happens if we pass `map` an effectful block argument? What is the return type of this call?

```
map([1, 2, 3]) { x =>
  do yield(x)
  x * 2
}
```


