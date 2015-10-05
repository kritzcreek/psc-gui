module Browser where

import           Prelude
import           Data.Maybe.Unsafe
import           Data.Nullable (toMaybe)
import           DOM (DOM())
import           DOM.HTML (window)
import           DOM.HTML.Document (body)
import           DOM.HTML.Types (htmlElementToElement)
import           DOM.HTML.Window (document)
import           DOM.Node.Types (Element())

import Psc.Gui.Component.Cwd
import Psc.Gui.Component.Load
import Psc.Gui.Component.Pursuit
import React
import qualified React.DOM as D

main = do
  body' <- getBody
  render ui body'
  where
    ui = D.div' [cwdF, loadF, pursuitF]
    getBody = do
      win <- window
      doc <- document win
      elm <- fromJust <$> toMaybe <$> body doc
      return $ htmlElementToElement elm
