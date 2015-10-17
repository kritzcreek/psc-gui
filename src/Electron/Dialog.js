//module Electron.Dialog


var dialog = window.require('remote').require('dialog')

exports.showOpenDialogImpl = function(options){
  return function(just){
    return function(nothing){
      return function(){
        var paths = dialog.showOpenDialog(options)
        return paths === undefined ? nothing : just(paths)
      }
    }
  }
}
