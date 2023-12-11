#!/usr/bin/env node

console.log("Running fasteffekt benchmarks (this may take a couple minutes)")
const runAll = require('./compare/comparator')
runAll()