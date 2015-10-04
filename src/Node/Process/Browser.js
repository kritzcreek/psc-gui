//module Node.Process.Browser

var process = electronRequire('remote').require('child_process')

var exec = function(cmd){
  return function(input){
    return function(){
      return process.execSync(cmd, {
        input: input,
        encoding: "utf-8"
      })
    }
  }
}

module.exports = {
  exec: exec
}
