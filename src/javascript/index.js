#!/usr/bin/env node

console.log("Running fasteffekt benchmarks (this may take a couple minutes)")
const path = require('path');
const runAll = require(path.resolve(__dirname, "./compare/comparator"))

const isVerify = (process.argv.length>2 && process.argv[2].toLowerCase() == "--verify")
console.log("is verify-mode:",isVerify)
runAll(isVerify)