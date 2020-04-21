# Pull-based Lexing
In this case study, we show how to implement a _pull-based_ lexer
in terms of effect handlers.

---

Before we get started, we require a few imports to deal with strings and regular expressions.
```
module examples/casestudies/lexer

import text/string
import text/regex
import immutable/option
import immutable/list
import mutable/array
```

## Tokens and Positions
First we define the datatypes to represent lexemes (tokens) and positions in the input stream:
```
record Position(line: Int, col: Int, index: Int)

type TokenType { Number(); Ident(); Punct(); Space() }

record Token(tpe: TokenType, text: String, position: Position)
```
Tokens simply are tagged with a token type (distinguishing numbers, identifiers, and punctuation),
the original text of the token and its position.

## The Lexer Effect
Next, we define the interface to the lexer as an effect signature.
```
effect Lexer {
  def peek(): Option[Token]
  def next(): Token
}
```
it consists of two effect operations, one to inspect the next token without consuming it (`peek`)
and one operation to advance in the stream of tokens. This describes the interface of a _pull-based_ lexer as a stream of tokens. Lexemes are only processed on demand.
An example program using the lexer effect is:

```
def example1() = {
  val t1 = next();
  val t2 = next();
  val t3 = next();
  (t1, t2, t3)
}
```

## Handling the Lexer Effect with a given List
A dummy lexer reading lexemes from a given list can be implemented as a handler for the `Lexer` effect. The definition uses the effect `LexerError` to signal the end of the input stream:
```
effect LexerError[A](msg: String, pos: Position): A
val dummyPosition = Position(0, 0, 0)

def lexerFromList[R](l: List[Token]) { program: R / Lexer }: R / LexerError = {
  var in = l;
  try { program() } with Lexer {
    def peek() = in match {
      case Nil() => resume(None())
      case Cons(tok, _) => resume(Some(tok))
    }
    def next() = in match {
      case Nil() => LexerError("Unexpected end of input", dummyPosition)
      case Cons(tok, _) => resume(tok)
    }
  }
}
```
We define a separate handler to report lexer errors to the console:
```
def report { prog: Unit / LexerError }: Unit / Console =
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
      println(example1())
    }
  }
```

## Handling the Lexer Effect by Processing a String
Of course, we can also implement a handler for our `Lexer` effect that _actually_
processes an input and computes the tokens contained therein.

This time, we use a number of different regular expressions to recognize lexemes.
First, we define the different token types as a list of pairs of regular expressions and token types.
```
record TokenRx(tpe: TokenType, rx: Regex)

val tokenDesriptors = [
  TokenRx(Number(), "^[0-9]+".regex),
  TokenRx(Ident(),  "^[a-zA-Z]+".regex),
  TokenRx(Punct(),  "^[=,.()\\[\\]{}:]".regex),
  TokenRx(Space(),  "^[ \t\n]+".regex)
]
```

```
def lexer[R](in: String) { prog: R / Lexer } : R / LexerError = {
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
      val lines = text.split("\n")
      val len = lines.unsafeGet(lines.size - 1).length
      // compute new positions
      index = index + text.length
      line = line + lines.size - 1
      if (lines.size == 1) { col = col + text.length } else { col = len }
    }
    def eos(): Boolean = index >= in.length
```
The function `tryMatch` applies a given token description to the current position of
the input stream, without advancing it. Its companion `tryMatchAll` returns the first token
matched by any of the matches in the given description list.
```
    def tryMatch(desc: TokenRx): Option[Token] =
        desc.rx.exec(input()).map { m => Token(desc.tpe, m.matched, position()) }

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
      println(example1())
    }
  }
```
yields the output:
```
//> (Token(Ident(), foo, Position(1, 1, 0)), Token(Punct(), (, Position(1, 4, 3)), Token(Punct(), ), Position(1, 5, 4)))
```

## Whitespace Skipping
Interestingly, a whitespace skipping lexer can be implemented as a _handler transformer_. That is, a handler that (partially) re-raises effect operations.

```
def skipWhitespace[R] { prog: R / Lexer }: R / Lexer =
  try { prog() } with Lexer {
    def peek() = resume(peek())
    def next() = next() match {
      case Token(Space(), text, pos) =>
        resume(next())
      case Token(tpe, text, pos) =>
        resume(Token(tpe, text, pos))
    }
  }
```
For simplicity of this example, `peek` does not skip whitespaces and still returns white space tokens.

```
def runExample3() =
  report {
    lexer("foo (   \n  )") {
      skipWhitespace {
        println(example1())
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