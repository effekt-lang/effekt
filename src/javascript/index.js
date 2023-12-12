#!/usr/bin/env node

console.log("Running fasteffekt benchmarks (this may take a couple minutes)")
console.log(__dirname)
const path = require('path');
const runAll = require(path.resolve(__dirname, "./compare/comparator"))
runAll()