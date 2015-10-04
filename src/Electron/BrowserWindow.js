//module Electron.BrowserWindow
var app = require('app');
var BrowserWindow = require('browser-window');
var R = require('ramda')

var newBrowserWindow = function(conf){
  return function(){
    return new BrowserWindow(conf)
  }
}

var loadUrl = function(bw, url){
    return function(){
      bw.loadUrl('file:///home/creek/Documents/psc-gui/index.html')
    }
}

module.exports = {
  newBrowserWindow: newBrowserWindow,
  loadUrl: R.curry(loadUrl)
}
