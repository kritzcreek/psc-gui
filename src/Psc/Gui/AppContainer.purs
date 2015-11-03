module Psc.Gui.Component.AppContainer where

import           Prelude
import qualified React.DOM as D
import qualified React.DOM.Props as DP
import qualified Thermite as T

import           Data.Either
import           Data.Foldable
import           Data.Lens
import           Data.Tuple


type ApplicationState =
  {
    cwd :: String,
    tabs :: Array String,
    activeTab :: Int
  }

_tabs = lens _.tabs (_ {tabs=_})
_TabbarAction = prism (uncurry TabbarAction) \aa ->
  case aa of
    TabbarAction i a -> Right (Tuple i a)
    _ -> Left aa

data ApplicationAction =
  TabbarAction Int TabbarAction
  | EditText String

app :: T.Render _ ApplicationState ApplicationAction
app dispatch _ state _  = [
  D.div' []
      ]

type TabbarState =
  {
    tabs      :: Array String,
    activeTab :: Int
  }

data TabbarAction =
  ClickTab Int

renderTabbar :: T.Render _ TabbarState TabbarAction
renderTabbar dispatch _ state children = [
  D.table' $ fold [T.withState \st ->
    T.focus _tabs _TabbarAction $
      T.foreach \_ -> T.simpleSpec sampleHandler sampleRender]
  ]

sampleHandler :: T.PerformAction _ _ _ _
sampleHandler _ _ _ _ = pure unit
sampleRender  _ _ _ _ = [D.div' [D.text "Hello"]]
