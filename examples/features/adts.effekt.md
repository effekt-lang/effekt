# Algebraic Datatypes

Defining a datatype in Effekt is similar to other languages.

```
type FileSystem {
  File(name: String, content: String)
  Directory(name: String, children: List[FileSystem])
}
```

Each definition starts with the `type` keyword and is followed by the name of the datatype. Enclosed by curly braces
and separated by newlines, variants consisting of a name and optional fields can be defined.

Creating instances of this type functions just as you might expect:

```effekt:repl
val file = File("README.md", "Effekt")
```

Even though, each variant looks like a record here, selectors like with records do not work here.

```effekt:repl
file.name // fails
```

Instead, pattern matching has to be used.

```effekt:hidden
def foldLeft[A, B](xs: List[A], init: B) { f: (B, A) => B }: B =
  xs match {
    case Nil => init
    case Cons(y, ys) => ys.foldLeft(f(init, y)) { f }
  }
```

```
def containsFile(fs: FileSystem, name: String): Bool =
  fs match {
    case File(name1, _) and name1 == name => true
    case File(_, _) => false
    case Directory(_, children) => 
      children
        .map { child => containsFile(child, name) }
        .foldLeft(false) { (lhs, rhs) => lhs || rhs } 
  }
```
