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
import Psc.Gui.Component.Completion
import Psc.Gui.Component.CardWrapper

import React
import qualified React.DOM as D
import qualified React.DOM.Props as P

main = do
  body' <- getBody
  render ui body'
  where
    ui = D.div [P.className "ui grid"] [
      D.div [P.className "three column row"] [
        cardWrapper "Completion" completionF,
        cardWrapper "Current Working Directory" cwdF,
        cardWrapper "Loading Modules" loadF,
        cardWrapper "Pursuit" pursuitF
      ]
    ]
    getBody = do
      win <- window
      doc <- document win
      elm <- fromJust <$> toMaybe <$> body doc
      return $ htmlElementToElement elm
