---
layout: docs
title: ANF Transformation
permalink: docs/casestudies/anf
---

# ANF Transformation
In this case study we implement a simple ANF transformation by using an effect to
non-locally insert binders for non-trivial expressions.

## Source Language

The source language of our transformation is the `Tree` data type from the
[parser case study](parser).

```
module examples/casestudies/anf

import examples/casestudies/parser // for the Tree datatype
import examples/casestudies/prettyprinter // for the combined example
```

To recall the Tree datatype, here is an example tree:
```
// let x = f(g(42)) in x
val exampleTree: Tree =
  Let("x", App("f", App("g", Lit(42))), Var("x"))
```

## Target Language
The target language, on the other side, is a
a language that distiguishes between effectful statements and pure expressions. We could of
course also use `Tree`. However, using a seaparate language that makes this distinction between
expressions and statements is more interesting, since the normal form is now encoded in the type.
```
type Expr {
  CLit(value: Int);
  CVar(name: String)
}
type Stmt {
  CLet(name: String, binding: Stmt, body: Stmt);
  CApp(name: String, arg: Expr);
  CRet(expr: Expr)
}
```
We prefix all constructors with `C...` to distinguish them from the source language ("C" for "Core").

## Utility Effects
Before we start with the definition of the transformation, we first define a utitly effect to
generate fresh names.
```
effect Fresh(): String
def freshVars[R] { prog: => R / Fresh } : R = {
    var i = 0;
    try { prog() }
    with Fresh { () => i = i + 1; resume("x" ++ show(i)) }
}
```

## ANF: The Bind Effect
Now to the core of this case study: the `Bind` effect. Here it becomes visible, why we chose
to transform into the language of `Stmt` and `Expr`. The `Bind` effect operation takes a
statement and somehow converts it into an expression.
```
effect Bind(e: Stmt): Expr
```
We define the ANF transformation from `Tree` to `Stmt` by calling `Bind` everytime
we encounter a statement, but would require an expression:
```
def traverse(e: Tree): Stmt / { Bind, Fresh } = e match {
  case Lit(n) => CRet(CLit(n))
  case Var(n) => CRet(CVar(n))
  case App(name, arg) =>
    // Here we use bind since other than App, CApp requires an expression
    CApp(name, do Bind(traverse(arg)))
  case Let(x, b, body) =>
    // here we use the handler `bindHere` to mark positions where bindings
    // should be inserted.
    CLet(x, bindHere { traverse(b) }, bindHere { traverse(body) })
}
```
The handler `bindHere` handles `Bind` by generating a fresh name and
inserting a let binding:
```
def bindHere { prog: => Stmt / Bind } : Stmt / Fresh =
  try { prog() }
  with Bind { (e) =>
    val id = do Fresh()
    CLet(id, e, resume(CVar(id)))
  }
```
Note, that the let binding will _enclose_ the overall result of `prog`. It is inserted on the outside.

The overall ANF transformation is simply a matter of composing the two handlers and calling `traverse`:
```
def translate(e: Tree): Stmt =
    freshVars { bindHere { traverse(e) } }
```

For our example, calling `translate` results in:
```
val exampleResult = translate(exampleTree)
//> CLet(x, CLet(x1, CRet(CLit(42)),
//    CLet(x2, CApp(g, CVar(x1)), CApp(f, CVar(x2)))),
//    CRet(CVar(x)))
```
which corresponds to `let x = (let x1 = 42 in let x2 = g(x1) in f(x2)) in x`.

## The Full Pipeline: Combining the Case Studies
So far, we have defined a lexer that handles the `Next` effect, a parser that uses
the `Next` effect and nondeterminism, a pretty printer that again uses some form
of nondeterminism, and finally an ANF transformation that non-locally inserts
bindings.

Of course, we can combine the different components into one bing pipeline. We start
by defining a pretty printer for the language of expressions and statements:

```
def toDocExpr(t: Expr): Unit / Pretty = t match {
    case CLit(value) => text(value.show)
    case CVar(name)  => text(name)
}

def toDocStmt(s: Stmt): Unit / Pretty = s match {
    case CLet(name, binding, body) =>
        text("let"); space(); text(name); space(); text("=");
        group {
            nested { line(); toDocStmt(binding) };
            line();
            text("in")
        };
        group { nested { line(); toDocStmt(body) } }

    case CApp(name, arg) =>
        text(name); parens {
            group { nested {
                linebreak();
                toDocExpr(arg)
            }; linebreak() }
        }

    case CRet(expr) =>
        text("return"); space(); toDocExpr(expr)
}

def pretty(s: Stmt) = pretty(40) { toDocStmt(s) }
```

Using the pretty printer, we can print our example result from above:
```
val examplePretty = pretty(exampleResult)
// let x = let x1 = return 42 in let x2 =
//      g(x1)
//    in f(x2) in return x
```

Finally, we can define our pipeline as lexing, parsing, transforming, and pretty printing
our input:
```
def pipeline(input: String): String =
  parse(input) { parseExpr() } match {
    case Success(tree) => pretty(translate(tree))
    case Failure(msg) => msg
  }
```

Here we use `pipeline` to translate some examples:
```
def main() = {
  println(exampleResult)
  println(examplePretty)

  println("----")
  println(pipeline("42"))

  println("----")
  println(pipeline("let x = 4 in 42"))

  println("----")
  println(pipeline("let x = let y = 2 in 1 in 42"))

  println("----")
  println(pipeline("let x = (let y = 2 in 1) in 42"))

  println("----")
  println(pipeline("let x = (let y = f(42) in 1) in 42"))
}
```
