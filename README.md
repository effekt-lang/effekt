# Effekt

Compared to other languages with effect handlers (and support for polymorphic effects) the Effekt language
aims to be significantly more lightweight in its concepts.

## Key Design Decisions

- no first class functions
- instead, we have parametrized (second-class blocks) that always have to be fully applied -- similar to blocks in Ruby
- blocks are first-order, they don't take other blocks
- function definitions have to be annotated with type signatures on the arguments (no ML style inference or let polymorphism)

The requirement that blocks always have to be (fully) applied and that we do not have first-class functions has the following implications:

- blocks only flow up the stack (they cannot be returned or escape otherwise)
- they are thus evaluated in a context / region tighter or equal to their definition context
- blocks do not need to be represented by closures -- all values used in blocks are still available on the call-stack

Maybe most importantly, effect checking becomes almost trivial, while supporting many use cases of effect polymorphism.


## Possible Language Extensions
There are many possible language extensions to grow Effekt into a GPL. When considering extensions, 
we need to check against our top one priority: simplicity. 
Are they really necessary? Do they work with the second class notion of blocks?

- ~val binding (without polymorphism!) -- should be easy~ (implemented)
- ~mutable variables, local control flow (branching, loops) -- should be easy~ (implemented)
- ~algebraic data types, pattern matching -- should be easy as long as constructors can only store values, no blocks~ (draft implemented)
- ~local function definitions -- while requiring FULL annotation of arguments. local functions can "close" over arguments / blocks. This is fine, since they are themselves not first class.~ (implemented)
- multi arity returns
- ~built in "show" and "equality"~ (implemented)
- type classes? Ad hoc polymorphism? overloading?

## Usage

### REPL
To start the REPL just clone the repository and enter `sbt run`. Checkout the `examples/` folder for a examples demonstrating features of the Effekt language.

### Compiling
To compile Effekt sources to JavaScript, provide the `--compile` flag. For example:

```
sbt run --compile myfile.effekt
```
This will generate JavaScript files in `./out`.

To run the generated JavaScript files you need to have a recent (> 10.0) version of Node.
The generated files currently use the `amdefine` module format.
To execute them you need to install `amdefine` and run it with nodeJS:

```
cd out
npm install amdefine
node
> require("./MY_FILE.js").main().run()
```

### Language Server
Effekt comes with a basic language server implementation (LSP). The `dist/vscode` folder contains a VSCode extension. 
There are at least two ways to setup VSCode. You can either download the `.vsix` file as Artifact from the latest commit, or build the VSCode editor extension yourself.
Both ways, however rely on a jar file that contains the Effekt implementation. Again, you can either download the `jar` file as Artifact, or build it yourself.

#### Downloading or Building the Jar File
Either download the `effekt.jar` or build it by running `sbt assembly`. The latter will generate the jar in the folder `target/scala-*/effekt.jar`. Remember the absolute path to the jar file, we need it later.

#### Downloading the VSCode Extension
Download the VSCode extension as Artifact from the latest commit. It should include a `vsix` file. In VSCode select "extensions / From VSIX ...".

#### Building the VSCode Extension
You first install the dependencies:
```
cd dist/vscode
npm install   
```
To build the `vsix` file, you also need to install `vsce`:
```
npm install -g vsce
```
Building the extension then amounts to running
```
vsce package
```
This will generate a `vsix` file that you can install as described above. You need to repeat this step everytime the extension changes.

#### Starting VSCode in Developer Mode
To automatically get latest updates (at the moment), you need to pretend you are a extension developer.
Again, first install the dependencies:
```
cd dist/vscode
npm install   
```
Then open VSCode from the `dist/vscode` folder.

Pressing `F5` will start a new VSCode instance with the extension loaded.

#### Configuring VSCode
To get the full language server support, you need to point the extension to the `jar` path you remembered above.

Open `settings` and search for `Effekt`. Under the setting `compiler` provide the absolute path to the `jar` file.
