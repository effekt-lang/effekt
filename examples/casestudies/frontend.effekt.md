---
layout: docs
title: Compiler Frontend Phases
permalink: docs/casestudies/frontend
---

# Compiler Frontend Phases

In this case study we collected multiple case studies useful and related to compiler frontend phases.
Namly, we showcase a pull-based lexer, a parser with backtracking behavior, a pretty printer and an ANF transformer.

---

# Pull-based Lexing
In this case study, we show how to implement a _pull-based_ lexer
in terms of effect handlers.

Before we get started, we require a few imports to deal with regular expressions.

```
import regex
```

## Tokens and Positions
First we define the datatypes to represent lexemes (tokens) and positions in the input stream:
```
record Position(line: Int, col: Int, index: Int)

type TokenKind { Number(); Ident(); Punct(); Space() }

def show(t: TokenKind): String = t match {
  case Number() => "number"
  case Ident()  => "identifier"
  case Punct()  => "punctuation"
  case Space()  => "space"
}

def infixEq(t1: TokenKind, t2: TokenKind): Bool =
  (t1, t2) match {
    case (Number(), Number()) => true
    case (Ident(), Ident()) => true
    case (Punct(), Punct()) => true
    case (Space(), Space()) => true
    case _ => false
  }

record Token(kind: TokenKind, text: String, position: Position)

def show(t: Token): String = t.kind.show

```
Tokens simply are tagged with a token type (distinguishing numbers, identifiers, and punctuation),
the original text of the token and its position.

## The Lexer Effect
Next, we define the interface to the lexer as an effect signature.
```
interface Lexer {
  def peek(): Option[Token]
  def next(): Token
}
```
it consists of two effect operations, one to inspect the next token without consuming it (`peek`)
and one operation to advance in the stream of tokens. This describes the interface of a _pull-based_ lexer as a stream of tokens. Lexemes are only processed on demand.
An example program using the lexer effect is:

```
def example1() = {
  val t1 = do next();
  val t2 = do next();
  val t3 = do next();
  (t1, t2, t3)
}
```

## Handling the Lexer Effect with a given List
A dummy lexer reading lexemes from a given list can be implemented as a handler for the `Lexer` effect. The definition uses the effect `LexerError` to signal the end of the input stream:
```
effect LexerError(msg: String, pos: Position): Nothing
val dummyPosition = Position(0, 0, 0)

def lexerFromList[R](l: List[Token]) { program: => R / Lexer }: R / LexerError = {
  var in = l;
  try { program() } with Lexer {
    def peek() = in match {
      case Nil() => resume(None())
      case Cons(tok, _) => resume(Some(tok))
    }
    def next() = in match {
      case Nil() => do LexerError("Unexpected end of input", dummyPosition)
      case Cons(tok, _) => resume(tok)
    }
  }
}
```
We define a separate handler to report lexer errors to the console:
```
def report { prog: => Unit / LexerError }: Unit =
  try { prog() } with LexerError { (msg, pos) =>
    println(pos.line.show ++ ":" ++ pos.col.show ++ " " ++ msg)
  }
```
Given a list of example tokens
```
val exampleTokens = [
  Token(Ident(), "foo", dummyPosition),
  Token(Punct(), "(", dummyPosition),
  Token(Punct(), ")", dummyPosition)
]
```
we can compose the two handlers to run our example consumer:
```effekt:repl
report {
  exampleTokens.lexerFromList {
    inspect(example1())
  }
}
```

## Handling the Lexer Effect by Processing a String
Of course, we can also implement a handler for our `Lexer` effect that _actually_
processes an input and computes the tokens contained therein.

This time, we use a number of different regular expressions to recognize lexemes.
First, we define the different token types as a list of pairs of regular expressions and token types.
```
record TokenRx(kind: TokenKind, rx: Regex)

val tokenDesriptors = [
  TokenRx(Number(), "^[0-9]+".regex),
  TokenRx(Ident(),  "^[a-zA-Z]+".regex),
  TokenRx(Punct(),  "^[=,.()\\[\\]{}:]".regex),
  TokenRx(Space(),  "^[ \t\n]+".regex)
]
```

```
def lexer[R](in: String) { prog: => R / Lexer } : R / LexerError = {
```
Additionally, we keep track of the current position in the input stream, by maintaining
three mutable variables for the zero based index, and one-based column and line position.
```
  var index = 0;
  var col = 1;
  var line = 1;
```
A few local helper functions ease the handling of the input stream.
At the same time, we need to keep track of the line information.
```
  def position() = Position(line, col, index)
  def input() = in.substring(index)
  def consume(text: String): Unit = {
    with ignore[MissingValue]
    val lines = text.split("\n")
    val offset = lines.last.length
    // compute new positions
    index = index + text.length
    line = line + lines.size - 1
    if (lines.size == 1) { col = col + text.length } else { col = offset }
  }
  def eos(): Bool = index >= in.length
```
The function `tryMatch` applies a given token description to the current position of
the input stream, without advancing it. Its companion `tryMatchAll` returns the first token
matched by any of the matches in the given description list.
```
  def tryMatch(desc: TokenRx): Option[Token] =
      desc.rx.exec(input()).map { m => Token(desc.kind, m.matched, position()) }

  def tryMatchAll(descs: List[TokenRx]): Option[Token] = descs match {
    case Nil() => None()
    case Cons(desc, descs) => tryMatch(desc).orElse { tryMatchAll(descs) }
  }
```
Now defining the lexer is trivial. We just need to use `tryMatchAll` and either consume
the input, or not.
```
  try { prog() } with Lexer {
    def peek() = resume(tryMatchAll(tokenDesriptors))
    def next() =
      if (eos())
        do LexerError("Unexpected EOS", position())
      else {
        val tok = tryMatchAll(tokenDesriptors).getOrElse {
          do LexerError("Cannot tokenize input", position())
        }
        consume(tok.text)
        resume(tok)
      }
  }
}
```
Running our above consumer with the string `"foo()"`
```effekt:repl
report {
  lexer("foo()") {
    inspect(example1())
  }
}
```
yields the output:
```
//> (Token(Ident(), foo, Position(1, 1, 0)), Token(Punct(), (, Position(1, 4, 3)), Token(Punct(), ), Position(1, 5, 4)))
```

## Whitespace Skipping
Interestingly, a whitespace skipping lexer can be implemented as a _effect transformer_. That is, a handler that (partially) re-raises effect operations.

```
def skipSpaces(): Unit / Lexer = do peek() match {
  case None() => ()
  case Some(Token(Space(), _, _)) => do next(); skipSpaces()
  case _ => ()
}

def skipWhitespace[R] { prog: => R / Lexer }: R / Lexer =
  try { prog() } with Lexer {
    def peek() = { skipSpaces(); resume(do peek()) }
    def next() = { skipSpaces(); resume(do next()) }
  }
```
The handler `skipWhitespace` simply skips all spaces by using the `Lexer` effect itself.

```effekt:repl
report {
  lexer("foo (   \n  )") {
    skipWhitespace {
      inspect(example1())
    }
  }
}
```

---

# Parsing
In this case study, we show how to implement a parser, using the lexer from the
[Lexer case study](lexer).

Parsers can be expressed by using the lexer effect and process the token stream. To model different alternatives in the grammar, we use the following effect for non-determinism:

```
interface Nondet {
  def alt(): Bool
  def fail(msg: String): Nothing
}

effect Parser = { Nondet, Lexer }
```

## Parser Combinators
Given these two effects, we can readily define a host of (imperative) parser combinators.
We start by the simplest one, which applies a predicate to the next element in the
input stream and fails, if it does not match.

```
def accept { p: Token => Bool } : Token / Parser = {
  val got = do next();
  if (p(got)) got
  else do fail("Unexpected token " ++ got.show)
}
```

Using `accept`, we can define parsers for the different token types.
```
def any() = accept { t => true }
def accept(exp: TokenKind) = accept { t => t.kind == exp }
def ident() = accept(Ident()).text
def number() = accept(Number()).text
def punct(p: String) = {
  val tok = accept(Punct())
  if (tok.text == p) ()
  else do fail("Expected " ++ p ++ " but got " ++ tok.text)
}
def kw(exp: String): Unit / Parser = {
  val got = ident();
  if (got == exp) ()
  else do fail("Expected keyword " ++ exp ++ " but got " ++ got)
}
```
Using the effect for non-deterministic choice `alt`, we can model alternatives, optional matches and various repetitions:
```
def or[R] { p: => R } { q: => R } =
  if (do alt()) { p() } else { q() }

def opt[R] { p: => R }: Option[R] / Parser =
  or { Some(p()) } { None() }

def many { p: => Unit }: Unit / Parser =
  or { some { p() } } { () }

def some { p: => Unit }: Unit / Parser =
  { p(); many { p() } }
```

## Example: A Simple Expression Language
We illustrate the usage of the above parser combinators by parsing a simple
expressions language.

```
type Tree {
  Lit(value: Int)
  Var(name: String)
  Let(name: String, binding: Tree, body: Tree)
  App(name: String, arg: Tree)
}
```

Let us start by defining the parser for numeric literals.
```
def parseNum(): Tree / Parser = {
  val numText = number();
  with default[WrongFormat, Tree] { do fail("Expected number, but cannot convert input to integer: " ++ numText) };
  Lit(numText.toInt)
}
```
We simply call the parser for `number()` and try to convert the
resulting string to an intenger.

```
def parseVar(): Tree / Parser =
  Var(ident())

def parseAtom() = or { parseVar() } { parseNum() }
```
Parsing variables is simply a matter of reusing the `ident` parser and changing the
result. Users of monadic parser combinator libraries might be delighted to see, that we
do not need to use `map` or something similar to transform the result. The parser is
simply written in direct style, still offering great flexibility in modifying the
semantics of effects.

Similarly, we can write parsers for let bindings, by sequentially composing
our existing parsers:
```
def parseLet(): Tree / Parser = {
  kw("let");
  val name = ident();
  punct("=");
  val binding = parseExpr();
  kw("in");
  val body = parseExpr();
  Let(name, binding, body)
}
```
Again, note how naturally the result can be composed from the individual results, much like
manually writing a recursive descent parser. Compared to handcrafted parsers, the imperative
parser combinators presented here offer a similar flexibility. At the same time, the semantics
of `alt` and `fail` is still left open, offering flexibility in the implementation of the actual underlying parsing algorithm.

We proceed to implement the remaining parsers for our expression language:
```
def parseGroup() = or { parseAtom() } {
  punct("(");
  val res = parseExpr();
  punct(")");
  res
}

def parseApp(): Tree / Parser = {
  val funName = ident();
  punct("(");
  val arg = parseExpr();
  punct(")");
  App(funName, arg)
}

def parseExpr(): Tree / Parser =
  or { parseLet() } { or { parseApp() } { parseGroup() } }
```

## Example: Combining Parsers and Local Mutable State
It is possible to combine the imperative parser combinators with
local mutable state. The implementation of local variables in Effekt is
designed to interact well with continuation capture and multiple resumptions.
This is important for use cases like the parser example where the continuation is
potentially called multiple times.

The following example implements an example
```
// <EXPR> ::= <NUMBER> | <IDENT> `(` <EXPR> (`,` <EXPR>)*  `)`
```
It uses local (mutable) variables to count the number of leafs as semantic action.
```
def parseCalls(): Int / Parser =
  or { number(); 1 } {
    var count = 1;
    ident();
    punct("(");
    count = count + parseCalls();
    many {
        punct(",");
        count = count + parseCalls()
    };
    punct(")");
    count
  }
```
Notice how the user defined combinator `many` feels like a built-in control operator
`while`.

## Backtracking Parsers
As mentioned above, so far we used the non-determinism effects, but did not specify
what they mean. We could implement depth-first parsing corresponding to recursive descent.
Alternatively, we also could implement breadth-first parsing. Further, in case of
ambiguities, we have the choice whether we want to recognize only the first result
or compute all possible alternative ways to recognize the input.

For this case study, we implement a depth-first parser, computing the first
successful result. Results are represented by the following datatype:
```
type ParseResult[R] {
  Success(t: R);
  Failure(msg: String)
}
```

The parsing algorithm is simply implemented as a handler for `Parser`.

```
def parse[R](input: String) { p: => R / Parser }: ParseResult[R] = try {
  lexer(input) { skipWhitespace { Success(p()) } }
} with Nondet {
  def alt() = resume(true) match {
    case Failure(msg) => resume(false)
    case Success(res) => Success(res)
  }
  def fail(msg) = Failure(msg)
} with LexerError { (msg, pos) =>
  Failure(msg)
}
```
The handler reuses the lexer implementation of the [Lexer case study](lexer). The lexer
raises a `LexerError` in case of an unexpected enf of the input stream or if it cannot
recognize a token. Those lexer errors are simply converted into failures, which the
parser can backtrack. To establish the backtracking behavior, it is important that the
lexer is executed _under_ the parser handler and not the other way around. Only this way
the lexer positions will be restored when calling the continuation a second time with `resume(false)`.


## Running the Examples
Having implemented a handler for the `Parser` effect, we can run our example "grammars" on some inputs.

```effekt:repl
locally {
  inspect(parse("42") { parseCalls() })
  inspect(parse("foo(1)") { parseCalls() })
  inspect(parse("foo(1, 2)") { parseCalls() })
  inspect(parse("foo(1, 2, 3, 4)") { parseCalls() })
  inspect(parse("foo(1, 2, bar(4, 5))") { parseCalls() })
  inspect(parse("foo(1, 2,\nbar(4, 5))") { parseCalls() })

  inspect(parse("}42") { parseExpr() })
  inspect(parse("42") { parseExpr() })
  inspect(parse("let x = 4 in 42") { parseExpr() })
  inspect(parse("let x = let y = 2 in 1 in 42") { parseExpr() })
  inspect(parse("let x = (let y = 2 in 1) in 42") { parseExpr() })
  inspect(parse("let x = (let y = f(42) in 1) in 42") { parseExpr() })
  inspect(parse("let x = (let y = f(let z = 1 in z) in 1) in 42") { parseExpr() })
}
```

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
interface Emit {
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
interface LayoutChoice {
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

```effekt:repl
locally {

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

---

# ANF Transformation
In this case study we implement a simple ANF transformation by using an effect to
non-locally insert binders for non-trivial expressions.

## Source Language

The source language of our transformation is the `Tree` data type from the
[parser case study](parser).

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

```effekt:repl
locally {
  inspect(exampleResult)
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
