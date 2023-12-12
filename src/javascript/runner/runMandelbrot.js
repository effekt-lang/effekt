const path = require('path');
const mandelbrot = require(path.resolve(__dirname, "../Mandelbrot"))
const measure = require(path.resolve(__dirname, "../Measure"))

const jsMulti = 10;
const run = () => {
  for (let i = 0; i < 2 * jsMulti ; i++) {
    mandelbrot.newInstance().innerBenchmarkLoop(750)
  }
}

console.log(measure(run)/jsMulti)