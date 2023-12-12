
const measure = (operation) => {
  var start = Date.now()
  operation()
  return Date.now() - start
}

module.exports = measure;