# Case Studies

This folder contains several case studies showcasing the Effekt language and how to use effects and handlers.

Some of the case studies build up on each other. For those, we recommend looking at them in the following order:

1. [Pull-based Lexer](lexer.md): A lexer implementation using effects to express _requesting_ the next token
   of an input stream.
2. [Backtracking Parser](parser.md): An implementation of an imperative parser combinator library, that uses the lexer.
3. [Pretty Printer](prettyprinter.md): A pretty printer that uses nondeterminism to search for a layout filling a given width.

Other, independent case studies:

- [Naturalistic DSLs](naturalisticdsls.md): Using effect handlers to model linguistic effects and express natural language examples.

---
_(Please do not be confused by the following, which is necessary since these Markdown files are executed as tests)_
```
def main() = ()
```
