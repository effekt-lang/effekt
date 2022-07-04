---
layout: docs
title: Pretty Printer
permalink: docs/casestudies/prettyprinter
---

# Pretty Printer
In this case study, we implement a (naive) pretty printer library that performs
backtracking to find the first layout that matches the screen width.
We build on the work by

> Linear, bounded, functional pretty-printing
> S. Doaitse Swierstra and Olaf Chitil. JFP 2009

but adapt it to the setting of effect handlers.
Furthermore, the library presented here is neither linear
(it uses simple backtracking), bounded (backtracking is arbitrary), nor functional (combinators are imperative style).

```
module examples/casestudies/prettyprinter

import examples/casestudies/parser // just needed for the example (Tree)
import immutable/option
import immutable/list
import text/string
```

Similar to the [parser case study](parser), the pretty printing library is based
on a non-deterministic description of layouts. In particular, to fill the horizontal
space as best as possible, we first try to  pretty print _horizontally_ and fall
back to _vertical_ mode, if not enough space is available. For example, the
following text overflows when trying to layout horizontally:

```
// -------
// hello world
```

Treating the space as a potential line break, we can try again, this time vertically.
```
// -------
// hello
// world
```

Pretty printing also often includes ways to _group_ parts of a document. If a group does not
fit horizontally, all of its elements will be layouted vertically:

```
// --------------
// hello (this (is too) long)
```
Even though the first two words of the group would fit, the document will be pretty printed as

```
// -------------
// hello (this
// is too
// long)
```

## Layout as Effects

The layout configuration is modeled by the following data types and effects:
```
type Direction { Horizontal(); Vertical() }

effect Indent(): Int
effect DefaultIndent(): Int
effect Flow(): Direction
```
The effect `Flow` represents the current layouting direction. Also the indentation of the
document depends on the context and is therefore modeled as an effect.

Computing the layout of a document to be pretty printed uses the above three effects:
```
effect Layout = { Indent, DefaultIndent, Flow }
```

## Output: A Stream of Layout Elements
Before we look at examples on how to use the `Layout` effect, we introduce yet another effect to
emit the layouted documents:
```
effect Emit {
  def emitText(content: String): Unit
  def emitNewline(): Unit
}
def text(content: String) = do emitText(content)
def newline() = do emitNewline()
```
This way, the resulting document is represented as a stream of `text` and `newline` events.

## Pretty Printing Combinators
Using the text emitter and the layout configuration, we can express simple functions that
express spacing between layout elements.

```
def space() =
  text(" ")

def spaces(n: Int) =
  if (n > 0) text(" ".repeat(n)) else ()
```
We can also express a function that, depending on the current flow of directions prints either
the supplied text or a newline:
```
def lineOr(replace: String) = do Flow() match {
  case Horizontal() =>
    text(replace)
  case Vertical() =>
    newline()
    text(" ".repeat(do Indent()))
}
```
In the case of a newline, it also outputs indentation
```
def line() =
  lineOr(" ")

def linebreak() =
  lineOr("")
```

We purposefully distinguish between potential linebreaks that are horizontally rendered as a space (`line`)
and those that are not rendered horizontally (`linebreak`).

## Indentation
Indentation can be configured by locally handling `Layout` and thereby changing the indentation:

```
// Uses `n` as the indentation in the given document
def in[R](n: Int) { doc: => R / Layout }: R / Layout =
  try { doc() }
  with Indent { () => resume(n) }

// Adds `j` to the indentation in the current document
def nest[R](j: Int) { doc: => R / Layout }: R / Layout =
  (do Indent() + j).in { doc() }

// Uses the default indentation to nest a document
def nested[R] { doc: => R / Layout }: R / Layout =
  nest(do DefaultIndent()) { doc() }
```

## Fixing Local Layout Choices: Grouping as Handlers
Layout combinators often include a way to group components. This way all components of a group
are _all_ layouted horizontally or vertically. Similarly, we can implement handlers that locally
fix the direction:

```
def in[R](dir: Direction) { doc: => R / Layout }: R / Layout =
  try { doc() }
  with Flow { () => resume(dir) }

def horizontal { p: => Unit / Layout }: Unit / Layout =
  Horizontal().in { p() }

def vertical { p: => Unit / Layout }: Unit / Layout =
  Vertical().in { p() }
```

This way, we can locally determine whether _all_ children of a group should be layouted either
horizontally or vertically. However, often we want to mark _choice points_ and then search for a solution
by trying out the different choices until a layout fits the screen width. To express these
choices, we thus add the following `LayoutChoice` effect:
```
effect LayoutChoice {
  def choice(): Direction
  def fail[A](): A
}
```

The `LayoutChoice` effect is very similar to the `Nondet` effect in the [parser case study](parser).

We define the following effect alias for pretty printing documents that depend on layout choices:
```
effect Pretty = { Emit, Layout, LayoutChoice }
```

Using layout choices, we can express the maybe most important pretty printing combinator:
```
def group { p: => Unit / Layout } =
  do choice().in { p() }
```
The `group` combinator expresses that depending on the result of `choice` we either layout all children
horizontally or vertically.

Using group, we can for instance express variants of `line` and `linebreak`:
```
def softline() =
  group { line() }

def softbreak() =
  group { linebreak() }
```
Regardless of the enclosing group, a `softline` inserts a linebreak, if the remainder does not fit into the available space.

## Examples
Using the combinators we can express examples from the paper by Swierstra and Chitil.

```
def example1(l: List[Int]) = {
  text("[");
  l.foreach { n =>
    text(show(n));
    text(",");
    line()
  };
  text("]")
}
```
This layouts a list by potentially adding a linebreak after each comma. Pretty printing the
list `[1, 2, 3, 4]` at width `5` gives:

```
// -----
// [1,
// 2, 3,
// 4, ]
```
For simplicity, we keep the trailing comma.

The following example represents the document `(Hi you)!!!`
```
def example2() = {
  group { text("Hi"); line(); text("you") };
  text("!!!")
}
```
which pretty printed at width `6` yields:
```
// ------
// Hi
// you!!!
```

The next example illustrates that revising the layout decision for the parent group can also lead to a
revision of the layout decision of the child group (due to indentation).
```
def example3() = {
  group {
    text("this");
    nest(9) {
      line();
      group { text("takes"); line(); text("four") }
    };
    line();
    text("lines")
  }
}
```
Pretty printing results in:

```
// ---------------
// this
//          takes
//          four
// lines
```


## Implementing the Pretty Printer
So far, we have seen how the different effects can be used to describe combinators. We are now ready
to actually implement pretty printing by providing the necessary handlers. We start by implementing
a handler for `LayoutChoice` that searches for the first successful layout:

```
def searchLayout[R] { p : => R / LayoutChoice }: Option[R] =
  try { Some(p()) }
  with LayoutChoice {
    def fail[A]() = None()
    def choice() = resume(Horizontal()).orElse { resume(Vertical()) }
  }
```
The handler is essentially the same as the backtracking implementation of the [parser case study](parser) but
using optional values to indicate success or failure of a layouting attempt. If layouting horizontally fails
for a particular choice-point, we resume a second time with `Vertical`.

Handling the output emitter is straightforward. Here, we simply store all emitted elements in a string:
```
def writer { p: => Unit / Emit } = {
  var out = "";
  try { p(); out } with Emit {
    def emitText(t) = { out = out ++ t; resume(()) }
    def emitNewline() = { out = out ++ "\n"; resume(()) }
  }
}
```
We can implement a handler for `Layout` by intercepting emit effects and keeping track of the current
column position.
```
def printer(width: Int, defaultIndent: Int) { prog: => Unit / { Emit, Layout } } : Unit / { Emit, LayoutChoice } = {
  // the position in the current line
  var pos: Int = 0;

  try { prog() }
  // we allow flow to be flexible on the top-level
  with Flow { () => resume(do choice()) }
  // indentation starts at 0
  with Indent { () => resume(0) }
  // simply handle the default indentation with a constant
  with DefaultIndent { () => resume(defaultIndent) }
```
Maybe most interestingly, here we update the current position and invoke the effect operation `fail`, if
the document exceeds the width. This will potentially cause backtracking and revision of a preceeding layout decision.
If the current text still fits the line, we simply re-emit it.
```
  with Emit {
    def emitText(t) = {
      pos = pos + t.length;
      if (pos > width) { do fail() }
      else { text(t); resume(()) }
    }
    def emitNewline() = { pos = 0; newline(); resume(()) }
  }
}
```

Finally, we can compose the different handlers to a single pretty printing handler:
```
def pretty(width: Int) { doc: => Unit / Pretty }: String = {
  val result = searchLayout { writer { printer(width, 2) { doc() } } };
  result.getOrElse { "Cannot print document, since it would overflow." }
}
```
For simplicity, we output a string "Cannot print ..." if the document does not fit the line width
and we cannot find a layout. The order of handlers is important: The handler `searchLayout`,
which performs backtracking needs to be the outermost one. This way the mutable variable in `writer`
which contains the currently printed document is reset to its previous state.


## Using the Pretty Printing Library

We can of course define some additional combinators to describe documents:
```
def parens { p: => Unit }: Unit / Pretty = {
  text("("); p(); text(")")
}

def braces { p: => Unit }: Unit / Pretty = {
  text("{"); p(); text("}")
}
```

and write a pretty printer for the example `Tree` from the [parser case study](parser):

```
def toDoc(t: Tree): Unit / Pretty = t match {
  case Lit(value) => text(show(value))
  case Var(name) => text(name)
  case Let(name, binding, body) =>
    text("let"); space(); text(name); space(); text("=");
    group {
      nested { line(); toDoc(binding) };
      line();
      text("in")
    };
    group { nested { line(); toDoc(body) } }

  case App(name, arg) =>
    text(name); parens {
      group { nested {
        linebreak();
        toDoc(arg)
      }; linebreak() }
    }
}
```

We can first use the parser from the [parser case study](parser) to obtain
a parse tree, which we then pretty print:

```
def parseAndPrint(text: String, width: Int): String =
  parse(text) { parseExpr() } match {
    case Success(tree) => pretty(width) { toDoc(tree) }
    case Failure(text) => text
  }
```

For example, we obtain
```
def example4() = parseAndPrint("let x = (let y = 2 in 1) in 42", 10)
// ----------
// let x =
//   let y =
//     2
//   in 1
// in 42
```

## Additional Examples

```
def main() = {

  println("-----");
  println(pretty(5) { example1([1,2,3,4]) });

  println("----------");
  println(pretty(10) { example1([1,2,3,4,5,6,7,8,9,1,2,3,4]) });

  println("----------")
  println(example4())

  def example4b() = {
    text("def"); space(); text("foo"); parens {
      group {
        nest(2) {
          linebreak();
          group { text("x"); text(":"); space(); text("Int"); text(",") };
          line();
          group { text("y"); text(":"); space(); text("String") }
        };
        linebreak()
      }
    }
  }
  def example3b() = {
    example4b();
    space();
    braces {
      group {
        nest(2) {
          line();
          text("var"); space(); text("z"); space(); text("="); space(); text("42"); text(";")
        };
        line()
      }
    }
  }

  def example6() = {
    group {
      text("this");
      nest(9) {
        line();
        group { text("takes"); line(); text("many"); line(); text("f") }
      };
      line();
      text("l")
    }
  }

  def example7() = {
    group {
      text("this");
      line();
      text("will");
      nest(9) {
        line();
        group { text("take"); line(); text("many") }
      };
      line();
      text("lines")
    }
  }

  def helloWorld() = {
    text("hello")
    line()
    text("world")
  }

  println("------------------------------");
  println(pretty(30) { example4b() });
  println("--------------------");
  println(pretty(20) { example4b() });

  println("----------");
  println(pretty(50) { example3b() });
  println(pretty(15) { example3b() });

  println("------");
  println(pretty(6) { example2() });

  println("---------------");
  println(pretty(15) { example3() });

  println("--------------");
  println(pretty(14) { example6() });

  println("--------------");
  println(pretty(14) { example7() })
}

```