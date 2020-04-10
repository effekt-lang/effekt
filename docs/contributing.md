---
layout: page
title: Contributing
section: contributing
position: 4
---

# How to Contribute
Yay, we are happy that you want to contribute to the language. Here is how
you get started:

## Building Effekt
While this document might age and outdate, the most current and up-to-date
instructions can be found in the
[Github Workflows file](https://github.com/b-studios/effekt/blob/master/.github/workflows/ci.yml).
This might be a bit tricky to read but contains all necessary information.

If you notice that something does not work as expected, please consider
opening an [issue](https://github.com/b-studios/effekt/issues). Also feel free to
update this page on [Github](https://github.com/b-studios/effekt/tree/master/docs/docs)
and opening a pull request.

### Software Requirements
You need to have the following software installed to build and use Effekt:

- JDK >= 1.8 and [Maven](https://maven.apache.org/)
- sbt (<https://www.scala-sbt.org>)
- Node.js (>= 10) and npm

Why three package management tools? The main build tool we use is Effekt,
but we use Maven to extract dependencies and aggregate license files of the
library dependencies. Finally, we use npm simply as a way to deploy the
language as an npm-package.

### Dependencies
Additionally, since Effekt uses Scala.js to compile to JavaScript,
you also need a Scala.js compatible fork of

- the Kiama library <https://github.com/b-studios/kiama>

Once you have sbt installed, you can obtain and build the fork as follows:
```
git clone git@github.com:b-studios/kiama.git
cd kiama
git checkout scala-js
sbt publishLocal
```
### Compiling the Effekt Compiler
Great, you made it so far! We can finally compile the Effekt project. For
this enter the sbt terminal:

```
sbt
sbt:root> project effektJVM
sbt:effekt> test
...
```
As mentioned before, the Effekt compiler can be run as a java program, but
also as a Scala program. Hence, we need a slightly complicate
[Cross-build setup](https://www.scala-js.org/doc/project/cross-build.html)
with two projects `effektJVM` and `effektJS`.

### Generating the Effekt Binary
The Effekt binary is actually just a simple wrapper that invokes `java -jar effekt.jar`.
To generate the jar-file and assembly everything simply run:

```
sbt deploy
```
Afterwards, you can find the `effekt.jar` in the `bin/` folder.

### Working on the "Standard Library"
The Effekt standard library is still in its infancy. If you want to work on
it, we recommend the following workflow:

Create a test file in `examples` that uses your new / improved library module, like
```
module examples/mynewmoduletester

import io/mynewmodule

def main() = mynewfunction()
```
Open `sbt` and run
```
sbt
sbt:root> project effektJVM
sbt:effekt> run examples/mynewmoduletester.effekt
...
```

This makes sure that you are always interacting with the most up-to-date version
of the standard library you are currently working on.
