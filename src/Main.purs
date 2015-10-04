module Main where

import Prelude
import Electron.BrowserWindow (newBrowserWindow, loadUrl)
import PscIde.Command
import Node.Process
import Control.Monad.Eff.Console
import Data.Either

config = {height: 100, width: 100}

main = do
  window <- newBrowserWindow config
  loadUrl window "/index.html"
  d <- cwd
  either log (\(Message m) -> log m) d
