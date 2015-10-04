//module Node.Process

var process = require('child_process')
var R = require('ramda');

var exec = function(cmd, input){
  return function(){
    return process.execSync(cmd, {
      input: input,
      encoding: "utf-8"
    })
  }
}

module.exports = {
  exec: R.curry(exec)
}
