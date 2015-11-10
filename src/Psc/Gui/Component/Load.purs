module Psc.Gui.Component.Load (loadF, LoadProps(..)) where

import Prelude
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Data.Foldable
import Data.Either
import Data.String (split)

import Node.Process
import Psc.Ide.Command
import Psc.Gui.Component.Util

import qualified Thermite as T

import qualified React as R
import qualified React.DOM as D
import qualified React.DOM.Props as P

data Action = Refresh | LoadModule | LoadDependency | InputChange String
type State = {modules :: Array String, moduleInput :: String}
type LoadProps = {}
type LoadEff eff = ( process :: PROCESS, console :: CONSOLE | eff )
initialState = {modules: [], moduleInput: "Module.Name"}

render :: forall eff. T.Render State LoadProps Action
render send _ state children = [ D.div [P.key "load"] [
  sbutton "blue"
    [P.onClick \_ -> send Refresh]
    [D.text "Refresh"],
  sinput
    [ P._type "text"
    , P.onChange \e -> send (InputChange (unsafeTargetValue e))
    , P.value state.moduleInput
    ],
  sbutton "green"
    [P.onClick \_ -> send LoadModule]
    [D.text "Load Module"],
  sbutton "green"
    [P.onClick \_ -> send LoadDependency]
    [D.text "Load Dependencies"],
  D.ul
    [P.style {maxHeight: 200, overflowY: "auto", overflowX: "hidden"}]
    (map (\m -> D.li' [D.text m]) state.modules)
  ] ]

performAction :: forall eff. T.PerformAction (LoadEff eff) State LoadProps Action
performAction Refresh _ s k = do
  mods <- listLoadedModules
  case mods of
    Right (Modules ms) -> k (s {modules=ms})
    Left err -> log err
performAction (InputChange i) _ s k = k (s { moduleInput=i })
performAction LoadModule _ s k = do
  res <- load [s.moduleInput] []
  either log (\(Message s) -> log s) res
performAction LoadDependency _ s k = do
  res <- load [] [s.moduleInput]
  either log (\(Message s) -> log s) res

spec :: forall eff. T.Spec (LoadEff eff) State LoadProps Action
spec = T.simpleSpec performAction render

loadF :: R.ReactElement
loadF = R.createFactory (T.createClass spec initialState) {}
