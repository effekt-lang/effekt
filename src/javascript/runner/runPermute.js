const path = require('path');
const permutationsModule = require(path.resolve(__dirname, "../Permute"))
const measure = require(path.resolve(__dirname, "../Measure"))

const run = () => {
  for (let i = 0; i < 100 * 1000; i++) {
    permutationsModule.newInstance().benchmark()
  }
}

console.log(measure(run)/1000)