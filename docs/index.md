---
layout: home
title:  "Home"
section: "section_home"
position: 1

features:
  - first: [
      "Effect Handlers",
      "(Algebraic) effect handlers let you define advanced control-flow structures like Generators as user libraries. Those libraries can be seamlessly composed.",
      "concepts/effect-handlers"]

  - second: [
      "Effect Safety",
      "A type- and effect system that does not get into your way. Rely on a simple, yet powerful effect system that guarantees all effects to be handled.",
      "concepts/effect-safety"]

  - third: [
      "Lightweight Effect Polymorphism",
      "No need to understand effect polymorphic functions or annotate them. Explicit effect polymorphism simply does not exist.",
      "concepts/effect-polymorphism"]
---

## Getting Started

Currently, the recommended way of getting started with Effekt is to download a pre-built relase.

- Visit the [Release Page on Github](https://github.com/b-studios/effekt/releases)
- Download `effekt.jar`

You can now start Effekt by running
```
java -jar path/to/effekt.jar
```
This assumes you have a Java JDK >= 1.8 and NodeJS >= 10.x installed.
For convience, you can then set up a script like [this one](https://github.com/b-studios/effekt/tree/master/bin) and
add it to your PATH.

### Running the REPL
Running Effekt without any additional arguments will bring you into the REPL:

```
  _____     ______  __  __     _    _
 (_____)   |  ____|/ _|/ _|   | |  | |
   ___     | |__  | |_| |_ ___| | _| |_
  (___)    |  __| |  _|  _/ _ \ |/ / __|
  _____    | |____| | | ||  __/   <| |_
 (_____)   |______|_| |_| \___|_|\_\\__|

 Welcome to the Effekt interpreter. Enter a top-level definition, or
 an expression to evaluate.

 To print the available commands, enter :help

>
```
Here, you can define functions and enter expressions to evaluate.
Executing commands in the REPL compiles the corresponding Effekt-programs
to Javascript (you can find them in `./out/`) and runs them with nodeJS.
