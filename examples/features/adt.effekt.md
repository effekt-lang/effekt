This example shows basic usage of algebraic datatypes.

```
module examples/features/adt
```

We also use some example effects, so declare them first
```
effect NoSuchElement(): Int
effect Done(): Unit
```


ADTs are declared with the following syntax:
```
type List {
  Cons(n: Int, l: List);
  Nil()
}
```
This defines a datatype of integer lists.


While selectors are immediately available for **records**, on a sumtype like `List`, we cannot
be sure that selecting will succeed. So we write our own selector here:

```
def first(l : List) = l match {
  case Cons(m, _) => m
  case Nil() => do NoSuchElement()
}
```

A helper function to generate lists of length `n` using `Done` to signal stopping.
```
def genlist { f: => Int / Done } : List = {
  var l = Nil();
  try {
    while (true) { l = Cons(f(), l) }
  } with Done { () => () };
  l
}
```

Effekt performs trampolining, so using recursion is stack-safe:
```
def foreach(l : List) { f: Int => Unit } : Unit =
  l match {
    case Nil() => ()
    case Cons(n, Cons(m, rest)) =>
      f(m);
      foreach(rest) { n => f(n) }
    case _ => ()
  }
```

Finally, we can run a few examples using our list datatype:
```
def main() = {

    var n = 10;
    val l1 = genlist { if (n > 0) { n = n - 1; n } else { do Done(); 0 } };

    val l = Cons(1, Cons(2, Cons(3, Nil())));

    foreach(l1) { n =>
        println(n)
    };

    try {
        println(first(Cons(42, Nil())))
    } with NoSuchElement { () => () }
}
```