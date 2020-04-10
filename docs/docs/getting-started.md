---
layout: docs
title: Getting Started
permalink: docs/getting-started
---

# Getting Started

### Compiling Programs with Effekt
To compile Effekt sources to JavaScript, provide the `--compile` flag. For example:

```
effekt --compile myfile.effekt
```
This will generate JavaScript files in `./out`, the output folder can be configured by providing arguments `--out ./otherdir`.

To run the generated JavaScript files you need to have a recent (> 10.0) version of Node.

```
node
> require("out/MY_FILE.js").main().run()
```

### Language Server
Effekt comes with a basic language server implementation (LSP). The `dist/vscode` folder contains a VSCode extension.
There are at least two ways to setup VSCode. You can either download the `.vsix` file from the latest release, or build the VSCode editor extension yourself.
Both ways, however rely on a jar file that contains the Effekt implementation. Again, you can either download the `jar`, or build it yourself.

#### Downloading or Building the Jar File
Either download the `effekt.jar` or build it by running `sbt deploy`. The latter will generate the jar in the folder `bin/effekt.jar`. Remember the absolute path to the jar file, we need it later.

#### Downloading the VSCode Extension
Download the VSCode extension from the [latest release](https://github.com/b-studios/effekt/releases/latest). It should include a `vsix` file. In VSCode select "extensions / From VSIX ...".

#### Building the VSCode Extension
You first install the dependencies:
```
cd dist/vscode
npm install
```
To build the `vsix` file, you also need to install [`vsce`](https://code.visualstudio.com/api/working-with-extensions/publishing-extension):
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
To get the full language server support, you need to point the extension to the `effekt.jar` file
of the Effekt compiler.

Open `settings` and search for `Effekt`. Under the setting `compiler` provide the absolute path to the `jar` file.
