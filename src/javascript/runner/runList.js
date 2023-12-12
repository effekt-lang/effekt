const path = require('path');
const listBenchmark = require(path.resolve(__dirname, "../List"))
const measure = require(path.resolve(__dirname, "../Measure"))

const run = () => {
  for (let i = 0; i < 1000 * 100; i++) {
    listBenchmark.newInstance().benchmark()
  }
}

console.log(measure(run)/100)