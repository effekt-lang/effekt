const { execSync } = require('child_process');
const fs = require('fs');


// List of shell commands
const commands = [
  ['permute', 'src/effekt/benchmark/permute.effekt', 'node src/javascript/runner/runPermute.js'],
  ['list', 'src/effekt/benchmark/list.effekt', 'node src/javascript/runner/runList.js'],
  ["mandelbrot", "src/effekt/benchmark/mandelbrot.effekt","node src/javascript/runner/runMandelbrot.js"]
  // Add more commands as needed
];

/**
 * synced execution, only returns once command is done.
 * @param {*} command shell command as string to execute
 * @param {*} onOutput (commandOutput) => Void callback function
 */
const execute = (command, onOutput) => {
  const output = execSync(command, { encoding: 'utf8' });
  onOutput(output.trim())
}

function executeCommands(commands, isVerify) {
  const outputs = [];
  commands.forEach((command, index) => {
    const performance = { name: command[0], effekt: {}, js: {} }
    outputs.push(performance)

    console.log("running benchmark:", performance.name)
    const dirtyCd = "cd " + __dirname + " && cd ../../.. && "
    const amount = " 3 "
    const verifyArgs = isVerify ? " --verify" : ""

    const [executableName, effektPath, jsCmd ] = command
    const effektCmd = dirtyCd+`effekt.sh -b ${effektPath} && ./out/${executableName} `+amount+verifyArgs 
    console.log("run: ", effektCmd)
    execute(effektCmd, (time) => performance.effekt = time)

    // run pure JS benchmark 
    execute(dirtyCd + jsCmd + amount + verifyArgs, (time) => performance.js = `[${parseFloat(time)}]`)
  });

  const analysis = outputs.map(mark => ({...mark, effekt: analyzeDurations(mark.effekt), js: analyzeDurations(mark.js)}))

  const outputFile = "fasteffekt_results.json"
  const resultString = JSON.stringify(analysis.map(perf => ({ ...perf, ratio: perf.effekt.sum / perf.js.sum })), null, 3)
  fs.writeFileSync(outputFile, resultString);
  console.log(`Command outputs saved to ${outputFile}`);
  console.log(analysis.map( mark => ({ name: mark.name, effekt: mark.effekt.sum, js: mark.js.sum, ratio: mark.effekt.sum/mark.js.sum })))
}

/**
 * analyze duration array
 * @param {string} durations : list of duration times for every benchmark run
 */
const analyzeDurations = (durations) => {
  durations = JSON.parse(durations);
  const sum = durations.reduce((accumulator, currentValue) => accumulator + currentValue, 0);
  const avg = sum/durations.length

  return { sum: sum, avg: avg, durations: durations }
}


const runAll = (isVerify) => executeCommands(commands, isVerify)
module.exports = runAll
// 
