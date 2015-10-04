module Electron.BrowserWindow where

import Prelude
import Control.Monad.Eff

foreign import data BrowserWindow :: *

type BrowserWindowConfig = {
  height :: Int,
  width :: Int
}

foreign import newBrowserWindow :: forall eff.
  BrowserWindowConfig -> Eff eff BrowserWindow

foreign import loadUrl :: forall eff.
  BrowserWindow -> String -> Eff eff Unit
