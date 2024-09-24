---
layout: docs
title: Datatypes
permalink: docs/tutorial/adts
---

# Algebraic Datatypes

Defining a datatype in Effekt is similar to other languages.

```
type FileSystem {
  File(name: String, content: String)
  Directory(name: String, children: List[FileSystem])
}
```

A datatype definition starts with the `type` keyword and is followed by the name of the datatype. 
Variants are enclosed by curly braces and separated by newlines (or semicolons) and consist of a name and a (non-optional) list of fields.

Constructing values of this datatype works as you might expect:

```effekt:repl
val file = File("README.md", "Effekt")
```

While records allow selecting fields directly, this is not true on variants of a datatype.

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

A pattern match (initiated with the keyword `match`) consists of one or more clauses delimited by `case`. 
There is an exhaustivity check helping you to handle all cases of the scrutinee. 
Pattern guards, starting with `and`, can be used to refine matches with further conditions that evaluate to a `Bool`. 
Optionally, like with `if` expressions, a `match` expression can be followed by an `else` branch to provide a default case. 
Instead of an `else` branch in a `match` expression, you may also use a wildcard pattern
(`case _ => ...`).

```effekt:repl
containsFile(Directory("/", file), "README.md")
```
Furthermore, you can use partial matches in positions where a `Bool` is expected, like here:

```effekt:repl
if (file is File(name, content)) println("The file " ++ name ++ " has a content length of " ++ show(content.length))
```

## Lambda match

In case there is a function definition, where a block is expected as argument, like here:

```
def withFile(name: String) { prog: FileSystem => Unit }: Unit = <> 
```

You can omit the argument if you want to pattern match on it directly:

```effekt:repl
withFile("README.md") {
  case File(_, _) => <>
  case Directory(_, _) => <>
}
```