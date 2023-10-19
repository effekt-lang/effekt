---
layout: docs
title: Parser
permalink: docs/casestudies/parser
---

# Parsing
In this case study, we show how to implement a parser, using the lexer from the
[Lexer case study](lexer).

---

Again, we require some imports -- in particular, we reuse the lexer implementation.
```
module examples/casestudies/parser

import examples/casestudies/lexer
import immutable/option
import text/string
```

Parsers can be expressed by using the lexer effect and process the token stream. To model different alternatives in the grammar, we use the following effect for non-determinism:

```
effect Nondet {
  def alt(): Boolean
  def fail[A](msg: String): A
}

effect Parser = { Nondet, Lexer }
```

## Parser Combinators
Given these two effects, we can readily define a host of (imperative) parser combinators.
We start by the simplest one, which applies a predicate to the next element in the
input stream and fails, if it does not match.

```
def accept { p: Token => Boolean } : Token / Parser = {
  val got = do next();
  if (p(got)) got
  else do fail("Unexpected token " ++ show(got))
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
def show(t: Tree): String = t match {
  case Lit(v) => "Lit(" ++ show(v) ++ ")"
  case Var(name) => "Var(" ++ show(name) ++ ")"
  case Let(n,b,body) => "Let(" ++ show(n) ++ ", " ++ show(b) ++ ", " ++ show(body) ++ ")"
  case App(n,a) => "App(" ++ show(n) ++ ", " ++ show(a) ++ ")"
}
```

Let us start by defining the parser for numeric literals.
```
def parseNum(): Tree / Parser = {
  val numText = number()
  val num = toInt(numText).getOrElse {
    do fail("Expected number, but cannot convert input to integer: " ++ numText)
  }
  Lit(num)
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
def showPR[A](p: ParseResult[A]){f: A => String}: String = p match {
  case Success(t) => "Success(" ++ f(t) ++ ")"
  case Failure(msg) => "Failure(" ++ msg ++ ")"
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
  def fail[A](msg) = Failure(msg)
} with LexerError[A] { (msg, pos) =>
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

```
def println(p: ParseResult[Int]): Unit = println(showPR(p){ x => show(x) })
def println(p: ParseResult[Tree]): Unit = println(showPR(p){ x => show(x) })
def main() = {
  println(parse("42") { parseCalls() })
  println(parse("foo(1)") { parseCalls() })
  println(parse("foo(1, 2)") { parseCalls() })
  println(parse("foo(1, 2, 3, 4)") { parseCalls() })
  println(parse("foo(1, 2, bar(4, 5))") { parseCalls() })
  println(parse("foo(1, 2,\nbar(4, 5))") { parseCalls() })

  println(parse("}42") { parseExpr() })
  println(parse("42") { parseExpr() })
  println(parse("let x = 4 in 42") { parseExpr() })
  println(parse("let x = let y = 2 in 1 in 42") { parseExpr() })
  println(parse("let x = (let y = 2 in 1) in 42") { parseExpr() })
  println(parse("let x = (let y = f(42) in 1) in 42") { parseExpr() })
  println(parse("let x = (let y = f(let z = 1 in z) in 1) in 42") { parseExpr() })
}
```