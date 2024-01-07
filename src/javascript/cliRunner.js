const measureMillis = (operation) => {
  var start = Date.now()
  operation()
  return Date.now() - start
}

const runFromCli = (miniBench, normalBench) => {
  // permute.js 10 --small
  const [file, path, iterationsString, isSmallFlag] = process.argv

  const isSmall =isSmallFlag?.toLowerCase() === "--small"
  const iterations = parseInt(iterationsString)
  if (!isSmall && isNaN(iterations)) {
    throw Error("cli runner was not given cmd arg for iterations or smallFlag!")
  }
  var durationLog = isSmall ?
    Array(3).fill(0).map(i => measureMillis(miniBench))
    :
    Array(iterations).fill(0).map(i => measureMillis(miniBench))
  return JSON.stringify(durationLog)
}

module.exports = { runFromCli }