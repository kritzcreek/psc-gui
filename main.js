require('module').globalPaths.push(__dirname + "/output");

var app = require('app');  // Module to control application life.
var BrowserWindow = require('browser-window');

var Node_Process = require('Node.Process');
var Node_Stream = require('Node.Stream');
// Report crashes to our server.
require('crash-reporter').start();

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is GCed.
var mainWindow = null;
var pscIdeServer = null;

// Quit when all windows are closed.
app.on('window-all-closed', function() {
  // On OS X it is common for applications and their menu bar
  // to stay active until the user quits explicitly with Cmd + Q
  if (process.platform != 'darwin') {
    Node_Process.kill(pscIdeServer)();
    app.quit();
  }
});

var runApp = function(){
  pscIdeServer = Node_Process.spawnPscIdeServer(__dirname)();
  Node_Stream.onData(pscIdeServer.stdout)(require('Control.Monad.Eff.Console').log)();
  mainWindow = new BrowserWindow({width: 800, height: 600});

  // and load the index.html of the app.
  mainWindow.loadUrl('file://' + __dirname + '/index.html');

  // Open the DevTools.
  mainWindow.openDevTools();

  // Emitted when the window is closed.
  mainWindow.on('closed', function() {
    // Dereference the window object, usually you would store windows
    // in an array if your app supports multi windows, this is the time
    // when you should delete the corresponding element.
    mainWindow = null;
  });
};

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
app.on('ready', runApp);

// var Main = require('./output/Main/index.js');
// app.on('ready', Main.main);
