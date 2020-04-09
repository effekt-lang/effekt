#! /usr/bin/env node
var shell = require('shelljs');
var spawn = require("child_process").spawn;

var java = "java"

// check whether Java is installed
if (!shell.which(java)) {
  shell.echo('Effekt requires a java installation in your path');
  shell.exit(1);
}

var userArgs = process.argv.slice(2);
var args = ["-jar", __dirname + '/effekt.jar'].concat(userArgs)

var effekt = spawn(java, args);

process.stdin.pipe(effekt.stdin);
effekt.stdout.pipe(process.stdout);
effekt.stderr.pipe(process.stderr);

effekt.on("close", function(code) {
  process.exit(code);
});
