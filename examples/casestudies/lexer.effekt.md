---
layout: docs
title: Lexer
permalink: docs/casestudies/lexer
---

# Pull-based Lexing
In this case study, we show how to implement a _pull-based_ lexer
in terms of effect handlers.

---

Before we get started, we require a few imports to deal with strings and regular expressions.
```
module examples/casestudies/lexer

import string
import text/regex
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
```
def runExample1() =
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
      desc.rx.exec(input()).map { m => Token(desc.kind, m.content, position()) }

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
```
def runExample2() =
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

```
def runExample3() =
  report {
    lexer("foo (   \n  )") {
      skipWhitespace {
        inspect(example1())
      }
    }
  }
```

### Running the Examples
To run this markdown file, simply supply its name as argument to the `effekt` binary.
```
def main() = {
  runExample1()
  runExample2()
  runExample3()
}
```
