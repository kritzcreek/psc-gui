module Psc.Gui.Component.Tabbar (tabbarF, TabbarProps(..)) where

import Prelude
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console

import Psc.Gui.Component.Util
import Psc.Gui.Component.Types

import qualified Thermite as T

import qualified React as R
import qualified React.DOM as D
import qualified React.DOM.Props as P

import qualified Signal.Channel as C

data Action = ClickTab Int
type State = { }
type TabbarProps = {
  tabs      :: Array String,
  activeTab :: Int,
  channel   :: C.Channel ToplevelAction
}

type TabbarEff eff = ( chan :: C.Chan | eff )
initialState = { }

renderTab :: _ -> Int -> Int -> String -> R.ReactElement
renderTab send activeTab tabIndex title =
  D.a
    [P.className (if activeTab == tabIndex then "item active" else "item"),
    P.onClick \_ -> send (ClickTab tabIndex)]
    [D.text title]

render :: forall eff. T.Render (TabbarEff eff) State TabbarProps Action
render send state props children =
  D.div
    [P.className "ui secondary pointing menu"]
    (zipWithEnumerated (renderTab send props.activeTab) props.tabs)

performAction :: forall eff. T.PerformAction (TabbarEff eff) State TabbarProps Action
performAction props (ClickTab x) = liftEff (C.send props.channel (ChangeTab x))

spec :: forall eff. T.Spec (TabbarEff eff) State TabbarProps Action
spec = T.simpleSpec initialState performAction render

tabbarF :: TabbarProps -> R.ReactElement
tabbarF props = R.createFactory (T.createClass spec) props
