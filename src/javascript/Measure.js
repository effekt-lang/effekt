
/**
 * will run operation and return the time it took to run
 * @param {()=>any} operation 
 * @returns 
 */
const measure = (operation) => {
  var start = Date.now()
  operation()
  return Date.now() - start
}

module.exports = measure;