---
layout: docs
title:  "Guides & Examples"
position: 3
---
<script>
document.onload = function() {
  hljs.registerLanguage("effekt", highlightEffekt);
}
</script>


# Guides & Examples

{% for x in site.pages %}
  {% if x.section == 'docs' and x.title != page.title %}
- [{{x.title}}]({{site.baseurl}}{{x.url}})
  {% endif %}
{% endfor %}


## Some Example with Syntax Highlighting

```effekt
module parser
```

```effekt
effect Flip(): Boolean
effect Next(): String
effect Fail[A](msg: String): A
effect Error[A](msg: String): A
```
This is some inline code: `foo(42)`
```effekt
type Stream {
  Cons(el: String, rest: Stream);
  EOS()
}

type Option[R] {
  Some(n: R);
  None()
}

type ParseResult[R] {
  Success(t: R);
  Failure(msg: String);
  ParseError(msg: String)
}
```
```effekt
def accept(exp: String) = {
    val got = do Next();
    if (got == exp) {
        got
    } else {
        do Fail("Expected " ++ exp ++ " but got " ++ got)
    }
}

def or[R] { p: R } { q: R } =
  if (do Flip()) { p() } else { q() }

def asOrB(): Int / { Flip, Next, Fail } =
  or { accept("a"); asOrB() + 1 } { accept("b"); 0 }

// Int = Result
def reader[R](s: Stream) { p : R / Next } : R / Fail = {
    var inn = s;
    try{
      p()
    } with {
      case Next() => inn match {
        case EOS() => { do Fail("Unexpected EOS") }
        case Cons(el, rest) => {
          inn = rest;
          resume(el)
        }
      }
    }
}

def eager[R] { p: R / { Flip, Fail, Error } }: ParseResult[R] = try {
  Success(p())
} with {
  case Flip() => resume(true) match {
    case Failure(msg) => resume(false)
    case Success(res) => Success(res)
    case ParseError(msg) => ParseError(msg)
  }
  case Fail(msg) => Failure(msg)
  case Error(msg) => ParseError(msg)
}

def commit[R] { p : R / Fail } : R / Error =
  try { p() } with {
    case Fail(msg) => { do Error(msg) }
  }

def nocut[R] { p: R / Error } : R / Fail =
  try { p() } with {
    case Error(msg) => { do Fail(msg) }
  }

def opt[R] { p: R }: Option[R] / Flip =
   or { Some(p()) } { None() }

def parse[R](s: Stream) { p : R / { Flip, Next, Fail, Error } } =
  eager {
    reader(s) {
      p()
    }
  }

def ex() =
  or {
    accept("do");
    commit { accept("foo"); 0 }
  } {
    accept("do");
    accept("bar");
    1
  }

def p1() = {
  val inn = Cons("a", Cons("a", Cons("a", Cons("b", EOS()))));
  parse(inn) { asOrB() } match {
    case Success(n) => println(n)
    case Failure(msg) => ()
    case ParseError(msg) => ()
  }
}

def p2() = {
  val inn = Cons("do", Cons("foo", EOS()));
  parse(inn) { ex() } match {
    case Success(n) => println(n)
    case Failure(msg) => ()
    case ParseError(msg) => ()
  }
}

def main() = {
    p1();
    p2()
}

```

```scala
def foo() = 42
```
