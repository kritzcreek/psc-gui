module Psc.Gui.Component.CardWrapper (cardWrapper) where

import Prelude

import qualified React as R
import qualified React.DOM as D
import qualified React.DOM.Props as P

cardWrapper :: String -> R.ReactElement -> R.ReactElement
cardWrapper title el = D.div [P.className "column"] [D.div [P.className "ui card"] [
  D.div [P.className "content"] [
    D.div [P.className "header"] [D.text title],
    el
  ]
]]
