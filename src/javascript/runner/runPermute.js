const permutationsModule = require('../Permute');
const measure = require("../Measure")

const run = () => {
  for (let i = 0; i < 100; i++) {
    permutationsModule.newInstance().benchmark()
  }
}

console.log(measure(run))