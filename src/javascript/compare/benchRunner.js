const path = require('path');
const measure = require(path.resolve(__dirname, "../Measure"))

const parseArgs = (args) => {
  if (args.length < 4) {
    throw new Error('Insufficient arguments');
  }
  const [file, path, benchmark, itsString, verify] = args
  console.log(args)
  const bench = getBenchmark(benchmark); 
  const iterations = parseIterations(itsString);

  console.log(iterations)
  console.log(bench())
  const durations = Array(iterations).map(a => measure(bench))
  console.log(JSON.stringify(durations))
};

const parseIterations = (its) => {
  return parseInt(its)
}

const getBenchmark = (benchmarkName) => {
  switch (benchmarkName) {
    case 'permute':
      // Logic for 'permute' command
      const permute = require(path.resolve(__dirname, "../Permute"))
      console.log("permute benchmark selected")
      return () => permute.newInstance().benchmark();
    case 'mandelbrot':
      // Logic for 'generate' command
      return { command: 'generate' };
    case 'nbody':
      // Logic for 'generate' command
      return { command: 'generate' };
    case 'list':
      // Logic for 'generate' command
      return { command: 'generate' };

    // Add more cases for other commands as needed
    default:
      throw new Error(`Unknown benchmark name: ${command}`);
  }
}

parseArgs(process.argv)
