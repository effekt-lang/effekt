# Pretty Printing
In this case study, we implement a (naive) pretty printer library that performs
backtracking to find the first layout that matches the screen width.

```
module examples/pos/pretty

import immutable/option
import immutable/list
import text/string
```

Similar to the [parser case study](parser.md), the pretty printing library is based
on a non-deterministic description of layouts. In particular, to fill the horizontal
space as best as possible, we first try to  pretty print horizontally and fall
back to vertical mode, if not enough space is available.

The layout configuration is modeled by the following data types:
```
type Direction { Vertical(); Horizontal() }
// TODO maybe split into two effects Flow and Indentation?
record LayoutConfig(indent: Int, defaultIndent: Int, dir: Direction)
```

Since the layout depends on the current context, we allow obtaining the current layout
configuration with an affect operation `Layout`:
```
effect Layout(): LayoutConfig
```

## Output: A Stream of Layout Elements
Before we look at examples on how to use the `Layout` effect, we introduce yet another effect to
emit documents:
```
effect Emit {
  def text(content: String): Unit
  def newline(): Unit
}
```
This way, the resulting document is represented as a stream of `text` and `newline` events.

## Pretty Printing Combinators
Using the text emitter and the layout configuration, we can express simple functions that
express spacing between layout elements.

```
def isHorizontal() = Layout().dir == Horizontal()
def isVertical() = Layout().dir == Vertical()
def currentIndent() = Layout().indent

def space() =
  text(" ")

def spaces(n: Int) =
  if (n > 0) text(" ".repeat(n)) else ()

def lineOr(replace: String) = (Layout().dir) match {
  case Horizontal() =>
    text(replace)
  case Vertical() =>
    newline()
    text(" ".repeat(currentIndent()))
}

def line() =
  lineOr(" ")

def linebreak() =
  lineOr("")
```

We purposefully distinguish between potential linebreaks that are horizontally rendered as a space (`line`)
and those that are not rendered horizontally (`linebreak`).

## Indentation
Indentation can be configured by locally handling `Layout` and changing the indentation:

```
def withIndent[R](n: Int) { p: R / Layout }: R / Layout =
  try { p () }
  with Layout { () => Layout() match {
    case LayoutConfig(ind, defInd, dir) => resume(LayoutConfig(ind + n, defInd, dir))
  }}

def nest[R](j: Int) { p: R / Layout }: R / Layout =
  withIndent(currentIndent() + j) { p() }

// uses the default indentation to nest a document
def nested[R] { p: R / Layout }: R / Layout =
  nest(Layout().defaultIndent) { p() }

// obtain the current nesting
def nesting[R] { p: Int => R / Layout } : R / Layout =
  p(Layout().defaultIndent)
```

## Fixing Local Layout Choices: Grouping as Handlers
Layout combinators often include a way to group components. This way all components of a group
are _all_ layouted horizontally or vertically. Similarly, we can implement handlers that locally
fix the direction:

```
def withDirection[R](dir: Direction) { p: R / Layout }: R / Layout =
  try { p () }
  with Layout { () => Layout() match {
    case LayoutConfig(ind, defInd, _) => resume(LayoutConfig(ind, defInd, dir))
  }}

def horizontal { p: Unit / Layout }: Unit / Layout =
  withDirection(Horizontal()) { p() }

def vertical { p: Unit / Layout }: Unit / Layout =
  withDirection(Vertical()) { p() }
```

This way, we can locally determine whether _all_ children of a group should be layouted either
horizontally or vertically. However, often we want to mark choice points and then search for a solution
by trying out the different choices until a layout fits the screen width. To express these
choices, we thus add the following `LayoutChoice` effect:
```
effect LayoutChoice {
  def choice(): Direction
  def fail[A](): A
}
```
The `LayoutChoice` effect is very similar to the `Nondet` effect in the [parser case study](parser.md).
Using it, we can express the maybe most important pretty printing combinator:

```
def group { p: Unit / Layout } =
  withDirection(choice()) { p() }
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

## Implementing the Pretty Printer
So far, we have seen how the different effects can be used to describe combinators. We are now ready
to actually implement pretty printing by providing the necessary handlers. We start by implementing
a handler for `LayoutChoice` that searches for the first successful layout:

```
def layout[R] { p : R / LayoutChoice }: Option[R] =
  try { Some(p()) } with LayoutChoice {
    def fail() = None()
    def choice() = resume(Horizontal()) match {
      case None() => resume(Vertical())
      case Some(r) => Some(r)
    }
  }
```
The handler is essentially the same as the backtracking implementation of the [parser case study](parser.md.


```
effect Pretty = { Emit, Layout, LayoutChoice }
```


```
def printer(width: Int, defaultIndent: Int) { prog: Unit / { Emit, Layout } } : Unit / { Emit, LayoutChoice } = {
  var pos: Int = 0;
  try { prog()
  } with Emit {
    def text(t) = {
      pos = pos + t.length;
      if (pos > width) { do fail() }
      else { text(t); resume(()) }
    }
    def newline() = {
      pos = 0;
      newline()
      resume(())
    }
  } with Layout {
      resume(LayoutConfig(0, defaultIndent, choice()))
  }
}
```

```
def writer { p: Unit / Emit } = {
  var out = "";
  try { p(); out } with Emit {
    def text(t) = { out = out ++ t; resume(()) }
    def newline() = { out = out ++ "\n"; resume(()) }
  }
}
```

```
def pretty(width: Int) { p: Unit / Pretty } = {
  val result = layout { writer { printer(width, 2) { p() } } };
  result.getOrElse { "Cannot print document, since it would overflow." }
}
```


## Using the Pretty Printing Library

We can of course define some additional combinators to describe documents:
```
def parens { p: Unit }: Unit / Pretty = {
  text("("); p(); text(")")
}

def braces { p: Unit }: Unit / Pretty = {
  text("{"); p(); text("}")
}
```

Using the combinators we can express examples from the paper

> Linear, bounded, functional pretty-printing
> S. Doaitse Swierstra and Olaf Chitil. JFP 2009

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
def example4() = {
  group { text("Hi"); line(); text("you") };
  text("!!!")
}

def example5() = {
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

... and a few more examples:

```
def main() = {

  println("----------");
  println(pretty(10) { example1([1,2,3,4,5,6,7,8,9,1,2,3,4]) });

  def example2() = {
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
  def example3() = {
    example2();
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

  println("------------------------------");
  println(pretty(30) { example2() });
  println("--------------------");
  println(pretty(20) { example2() });

  println("----------");
  println(pretty(50) { example3() });
  println(pretty(15) { example3() });

  println("------");
  println(pretty(6) { example4() });

  println("---------------");
  println(pretty(15) { example5() });

  println("--------------");
  println(pretty(14) { example6() });

  println("--------------");
  println(pretty(14) { example7() })
}

```