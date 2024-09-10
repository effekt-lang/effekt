---
layout: docs
title: Datatypes
permalink: docs/tutorials/adts
---

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

Creating instances of this datatype functions just as you might expect:

```effekt:repl
val file = File("README.md", "Effekt")
```

Even though, each variant looks like a record here, selectors like with records do not work here.

```effekt:repl
file.name // fails
```

Instead, pattern matching has to be used.

```
def containsFile(fs: FileSystem, name: String): Bool =
  fs match {
    case File(name1, _) and name1 == name => true
    case Directory(_, children) => 
      children.any { c => containsFile(c, name) }
  } else { false }
```

A pattern match with `match` consists of one or more clauses delimited by `case`. There is an exhaustivity check such 
that you need to handle all cases of the scrutinee. Pattern guards, starting with `and`, can be used to refine matches
with further conditions that evaluate to a `Bool`. Optionally, like with `if` expressions, a `match` expression can be 
followed by an `else` branch. Instead of an `else` branch in a `match` expression, you may also use a wildcard pattern
(`case _ => ...`).

```effekt:repl
containsFile(Directory("/", file), "README.md")
```
Furthermore, you can use partial matches in positions where a `Bool` is expected, like here:

```effekt:repl
if (file is File(name, content)) println("The file " ++ name ++ " has a content length of " ++ show(content.length))
```
