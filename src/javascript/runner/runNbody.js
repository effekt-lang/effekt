const path = require('path');
const nbody = require(path.resolve(__dirname, "../Nbody"))
const measure = require(path.resolve(__dirname, "../Measure"))

const run = () => {
  for (let i = 0; i < 2 ; i++) {
    nbody.newInstance().innerBenchmarkLoop(250000)
  }
}

console.log(measure(run))