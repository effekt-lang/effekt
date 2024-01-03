#!/usr/bin/env node

const { verify } = require('crypto');
const path = require('path');
const runAll = require(path.resolve(__dirname, "./compare/comparator"))

const arg = process.argv.length > 2 ? process.argv[2].toLowerCase() : ""
const isHelp = (arg == "--help" || arg == "-h")

if (isHelp) {
  console.log(`
  fasteffekt - by Maximilian Marschall
  benchmarking the current install of the effekt language.
  will execute benchmarks in effekt and JS.
  outputs results to console and JSON file
  options:
    --help, -h:      documentation
    --small, -s:     run minimal benchmark to verify they all work
    --version, -v:   show version of fasteffekt
  `)
} else if (arg == "--version" || arg == "-v") {
  const packageJson = require('../../package.json');
  // Access the version field and log it
  console.log('Package version:', packageJson.version);
} else {
  const isVerify = (arg == "--small" || arg == "-s")
  if (isVerify)
    console.log("verify-mode:", isVerify)
  runAll(isVerify ? "--verify" : "")
}

