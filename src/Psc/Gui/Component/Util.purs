module Psc.Gui.Component.Util where

import Prelude

import Unsafe.Coerce

import qualified React as R
import qualified React.DOM as D
import qualified React.DOM.Props as P

sbutton ::
  String ->
  Array P.Props ->
  Array R.ReactElement ->
  R.ReactElement
sbutton classes props children = D.button
  ([P.className ("ui button " ++ classes)] ++ props)
  children

sinput ::
  Array P.Props ->
  R.ReactElement
sinput props = D.div [P.className "ui input"]
  [D.input props []]

unsafeTargetValue :: forall a. a -> String
unsafeTargetValue ev = (unsafeCoerce ev).target.value
