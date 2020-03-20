# Effekt Visual Studio Code

VSCode Language Server Client for Effekt

## Features

On file open and save, the Effekt compiler will be run and any resulting diagnostics will be displayed.
See the extension settings for additional features.

## Running

This extension is not currently published on the extension marketplace so you will have to install it manually in your extensions or run it from VSCode.
Before running, you will need to use the sbt command `assembly` in the Effekt repository to generate a
JAR with all necessary components. Alternatively, you can also download the JAR file as a published artifact
from github.

Then check the settings in VSCode. The setting `compiler` should contain an absolute path
to the jar file.

With this setup the extension should start the server when an Effekt file is opened.
