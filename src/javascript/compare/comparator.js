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
    const performance = { name: command[0], effekt: -1, js: -1 }
    outputs.push(performance)
    console.log("running benchmark:", performance.name)
    const dirtyCd = "cd " + __dirname + " && cd ../../.. && "
    const verifyArgs = isVerify ? " --verify" : ""

    const [executableName, effektPath, jsCmd ] = command
    const effektCmd = `effekt.sh -b ${effektPath} && ./out/${executableName} ` 
    execute(dirtyCd + effektCmd + verifyArgs, (time) => performance.effekt = time)


    execute(dirtyCd + jsCmd + verifyArgs, (time) => performance.js = time)
  });

  const outputFile = "fasteffekt_results.json"
  const resultString = JSON.stringify(outputs.map(perf => ({ ...perf, ratio: perf.effekt / perf.js })), null, 3)
  fs.writeFileSync(outputFile, resultString);
  console.log(`Command outputs saved to ${outputFile}`);
  console.log(resultString)
}

const runAll = (isVerify) => executeCommands(commands, isVerify)
module.exports = runAll
// 
