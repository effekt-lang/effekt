# Naturalistic DSLs
This document is based on Chapter 7. Discussion from the PhD Thesis:

> _"Design and Implementation of Effect Handlers for Object-Oriented Languages"_\
> Jonathan Immanuel Brachthäuser, Universität Tübingen (2019).

_(We translated it to the Effekt language, revised it and added more explanations and additional examples.)_

---

It is the goal of domain specific languages (DSLs) to bridge the conceptual
gap between language used in the particular domain and the computer language
domain experts use to solve the domain problems ([Hudak, 1996][@hudak96building]).
Domain language is often close to natural (that is, spoken) language, making
the task of DSL design particularly challenging ([Lopes et al., 2003][@lopes2003beyond]).
Natural language has the reputation of being lexically and syntactically
ambiguous, having complicated and context dependent binding structures,
and often has non-trivial semantics, which rarely is compositional.

In consequence, often DSLs are still far away from
being close to natural language. This is in particular the case for
DSLs, which are _embedded_ ([Hudak, 1996][@hudak96building]) into a general-purpose language. With
embedded DSLs, the host language additionally imposes its own
syntactical restrictions and typing discipline on the DSL designer.
Many limitations have been addressed in their own line of work. Examples include
syntax extensions as libraries ([Erdweg et al., 2011][@erdweg11sugarj]) and domain
specific type system extensions.
However, non-context-free linguistic constructs are often neglected.

In about the last decade, many developments in modeling the semantics
of natural languages have been inspired by computer science and
the theory of abstract machines and
control operators in particular. Importantly, [Chung-chieh Shan][@shan2005linguistic]
describes "noncompositional phenomena in natural languages" as
_linguistic side effects_.
Delimited continuations
have successfully been used to model linguistic side effects, such as
quantification, focus, and polymorphic coordination (
  [Shan, 2004][@shan2004delimited] and[2005][@shan2005linguistic]; [Barker, 2004][@barker2004continuations]).
[Maršík and Amblard][@marvsik2016introducing] recently used algebraic
effects with handlers to give a compositional semantics to deixis
(_"John loves *me*"_), quantification with scope islands (_"John loves *every woman*"_), and
implicature (_"John, *my best friend*, loves me"_).

Implementors of (domain specific) programming languages often reside to
effects to describe the _semantics_ of the language.
Inspired by the recent developments in natural language semantics, here we
propose to follow [Maršík and Amblard][@marvsik2016introducing] and use effects and handlers to describe
the _syntax_ of a DSL. In particular, we group the different
syntactic constructs of a DSL according to the following aspects:

1. _pure syntax_, that can be understood as compositional construction of
      the abstract syntax tree.
2. _effectful syntax_, that, like linguistic side effects ([Shan, 2005][@shan2005linguistic]), (often) requires
      context for interpretation and results in some form of non-local rewriting of the
      syntax tree. Effect operations can be used to express effectful syntax.
3.  _binding syntax_, which provides the necessary context. Binding syntax can be expressed as effect handlers for effectful syntax.


## Effectful Syntax in Effekt
Effekt comes equipped with a few features that allow designing elegant embedded
DSLs. One example is the infix notation of function application. Another one,
as we will see, is effect handlers, which we can use
to implement effectful DSLs.
To illustrate the gained expressivity, we implement examples from [Maršík and Amblard][@marvsik2016introducing]
as an embedded domain specific language. [Maršík and Amblard][@marvsik2016introducing] already
use a calculus of effects and handlers to express the semantics. We simply
translate the examples to Effekt.

### The Language of Sentence
We start by describing the language (DSL) of nominal phrases as the following datatype:

```
record Person(name: String)
```

Let us define some example people:
```
val John = Person("John")
val Peter = Person("Peter")
val Mary = Person("Mary")
```

Next, we define the sentence DSL:
```
type Sentence {
  Say(speaker: Person, sentence: Sentence)
  Is(person: Person, predicate: Predicate)
   // used later:
  ForAll(individual: Person, s: Sentence)
  Implies(a: Sentence, b: Sentence)
}
type Predicate {
  InLoveWith(p: Person)
  Woman()
}
def loves(lover: Person)(loved: Person) = Is(lover, InLoveWith(loved))
```

We can now construct sentences like

> John said "Mary loves me"

as:

```
val s1 = Say(John, Mary.loves(John))
```

### The Speaker Effect
However, there is a better way to express this: Using effects.
We define the speaker effect to refer to the contextual speaker of the sentence.

```
effect Speaker {
    def me(): Person
}
```
The semantics of `me` depends on the context. If we treat quotes in the above sentence as
scope, we see that "said" _handles_ me. That is:

```
def said(p: Person) { s: Sentence / Speaker }: Sentence / {} =
  try { Say(p, s()) } with Speaker { def me() = resume(p) }
```

Here is another definition of `said`, that does _not_ handle the speaker effect, corresponding
to omitting the quotationmarks:
```
def said(p: Person)(s: Sentence): Sentence =
  Say(p, s)
```

We can now express our example sentence as:
```
def s1a() = John.said { Mary.loves(me()) }
```
Note, that by overloading `said` we can also simply express the sentence

> John said Mary loves me

as
```
def s1b() = John.said ( Mary.loves(me()) )
```

Comparing the inferred types of `s1a` and `s1b` we see:
```
def s1aTpe(): Sentence / {} = s1a()
def s1bTpe(): Sentence / Speaker = s1b()
```
That is, `s1b` still has the speaker effect, while s1a is pure and
all linguistic effects are handled. We can handle `s1b` by adding the speaker:

```
def s1c() = Peter.said { s1b() }
```
which results in
```
//> Say(Person(Peter),
//    Say(Person(John),
//      Is(Person(Mary), InLoveWith(Person(Peter)))))
```

This simple example already illustrates that:

- effects can be used to model contextual information in natural language
- the effect system reminds us to actually handle (that is, provide semantics to) linguistic effects


### The `Scope` effect
Passing down context information, as we did with the speaker effect,
does not require full handlers. In Scala, for example, implicit
parameters would suffice to express this effect.
Things become more interesting when we consider the scope effect,
which can be used to model universal quantification ([Maršík and Amblard, 2016][@marvsik2016introducing]).

Instead of modeling `scope` directly as an effect, here we treat quantification as the effect:
```
effect Quantification {
  def every(who: Predicate): Person
}
```
We can use it as follows:
```
def s2(): Sentence = scoped { John.said { every(Woman()).loves(me()) } }
```
The effect operation `every` takes a predicate (&ie;, `Woman`)
and introduces a universal quantification at the position of the handler `scoped`.

The quantification effect is handled by `scoped`:
```
def scoped { s: Sentence / Quantification }: Sentence = {
  var tmp = 0;
  def fresh(): Person = { val x = Person("x" ++ show(tmp)); tmp = tmp + 1; x }
  try { s() } with Quantification {
    def every(who) = {
      val x = fresh()
      ForAll(x, Implies(Is(x, who), resume(x)))
    }
  }
}
```
This is already a more involved handler. It generates a fresh person name and
then systematically rewrites the syntax tree, moving the introduced
binder and the predicate up to the handler:
```
//> ForAll(Person(x0), Implies(Is(Person(x0), Woman()),
//    Say(Person(John), Is(Person(x0), InLoveWith(Person(John))))))
```
Every invocation of the effect operation `every` introduces an additional
quantifier.
This non-local rewriting of the syntax tree to introduce a binder is very
similar to let-insertion ([Yallop, 2017][@yallop2017staged]).
[Yallop][@yallop2017staged] shows how to use effect handlers to perform let-insertion.

### Running the Examples
Finally, we can run our examples to inspect the generated sentences.
```
def main() = {
  println(s1)
  println(s1a())
  println(s1c())
  println(s2())
}
```

[@hudak96building]: https://dl.acm.org/doi/10.1145/242224.242477
[@lopes2003beyond]: https://dl.acm.org/doi/abs/10.1145/966051.966058
[@marvsik2016introducing]: https://hal.archives-ouvertes.fr/hal-01079206/
[@erdweg11sugarj]: https://dl.acm.org/doi/abs/10.1145/2048066.2048099
[@shan2005linguistic]: http://homes.sice.indiana.edu/ccshan/dissertation/book.pdf
[@shan2004delimited]: https://arxiv.org/abs/cs/0404006
[@barker2004continuations]: https://www.nyu.edu/projects/barker/barker-cw.pdf
[@yallop2017staged]: https://dl.acm.org/doi/abs/10.1145/2847538.2847546