//module Electron.Dialog


var dialog = window.require('remote').require('dialog')

exports.showOpenDialogImpl = function(options){
  return function(){
    return dialog.showOpenDialog(options)
  }
}
