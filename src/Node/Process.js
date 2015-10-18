//module Node.Process

var child_process = process.type === 'browser' ?
      require('child_process') : window.require('remote').require('child_process')

var exec = function(cmd){
  return function(input){
    return function(){
      return child_process.execSync(cmd, {
        input: input,
        encoding: "utf-8"
      })
    }
  }
}

var spawn = function(cmd){
  return function(args){
    return function(){
      return child_process.spawn(cmd, args)
    }
  }
}

module.exports = {
  exec: exec,
  spawn: spawn
}
