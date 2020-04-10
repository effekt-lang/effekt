# Effekt

Compared to other languages with effect handlers (and support for polymorphic effects) the Effekt language
aims to be significantly more lightweight in its concepts.


## Disclaimer: Use at your own risk

**Effekt is a research-level language. We are actively working on it and the language (and everything else) is very likely to change.**

Also, Effekt comes with no warranty and there are (probably) many bugs -- If this does not discourage you, feel free to
play with it and give us your feedback :)

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

## Usage

### Preliminaries
You need to have the following software installed to build and use Effekt:

- java and maven
- sbt
- node and npm

### REPL
To start the REPL just clone the repository and enter `sbt run`. Checkout the `examples/` folder for examples demonstrating features of the Effekt language.

### Compiling
To compile Effekt sources to JavaScript, provide the `--compile` flag. For example:

```
sbt run --compile myfile.effekt
```
This will generate JavaScript files in `./out`, the output folder can be configured by providing arguments `--out ./otherdir`.

To run the generated JavaScript files you need to have a recent (> 10.0) version of Node.

```
cd out
node
> require("./MY_FILE.js").main().run()
```

### Language Server
Effekt comes with a basic language server implementation (LSP). The `dist/vscode` folder contains a VSCode extension. 
There are at least two ways to setup VSCode. You can either download the `.vsix` file from the latest release, or build the VSCode editor extension yourself.
Both ways, however rely on a jar file that contains the Effekt implementation. Again, you can either download the `jar`, or build it yourself.

#### Downloading or Building the Jar File
Either download the `effekt.jar` or build it by running `sbt deploy`. The latter will generate the jar in the folder `bin/effekt.jar`. Remember the absolute path to the jar file, we need it later.

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
