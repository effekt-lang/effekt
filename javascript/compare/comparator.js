const { execSync } = require('child_process');
const fs = require('fs');

// List of shell commands
const commands = [
  ['Permute','effekt.sh src/runner/runPermute.effekt', 'node javascript/runner/runPermute.js'],
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

function executeCommands(commands) {
  const outputs = [];
  commands.forEach((command, index) => {
    const performance = { name: command[0], effekt: -1, js: -1 }
    outputs.push(performance)
    execute(command[1], (time) => performance.effekt = time)
    execute(command[2], (time) => performance.js = time)
  });

  const outputFileName = 'commandOutputs.txt';
  fs.writeFileSync(outputFileName, JSON.stringify(outputs.map(perf => ({ ...perf, ratio: perf.effekt / perf.js })), null, 3));
  console.log(`Command outputs saved to ${outputFileName}`);
}

executeCommands(commands)

// 
