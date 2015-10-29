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
import qualified Thermite.Action as T

import qualified React as R
import qualified React.DOM as D
import qualified React.DOM.Props as P

data Action = Refresh | LoadModule | LoadDependency | InputChange String
type State = {modules :: Array String, moduleInput :: String}
type LoadProps = {}
type LoadEff eff = ( process :: PROCESS, console :: CONSOLE | eff )
initialState = {modules: [], moduleInput: "Module.Name"}

render :: forall eff. T.Render (LoadEff eff) State LoadProps Action
render send state props children = D.div [P.key "load"] [
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
  ]

performAction :: forall eff. T.PerformAction (LoadEff eff) State LoadProps Action
performAction _ Refresh = do
  mods <- liftEff $ listLoadedModules
  case mods of
    Right (Modules ms) -> T.modifyState (\s -> s {modules=ms})
    Left err -> liftEff (log err)
performAction _ (InputChange i) = T.modifyState (\s -> s {moduleInput=i})
performAction _ LoadModule = do
  i <- T.getState
  res <- liftEff (load [i.moduleInput] [])
  liftEff (either log (\(Message s) -> log s) res)
performAction _ LoadDependency = do
  i <- T.getState
  res <- liftEff (load [] [i.moduleInput])
  liftEff (either log (\(Message s) -> log s) res)

spec :: forall eff. T.Spec (LoadEff eff) State LoadProps Action
spec = T.componentWillMount Refresh $
  T.simpleSpec initialState performAction render

loadF :: R.ReactElement
loadF = R.createFactory (T.createClass spec) {}
