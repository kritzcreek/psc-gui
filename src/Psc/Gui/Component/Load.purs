module Psc.Gui.Component.Load (loadF, LoadProps(..)) where

import Prelude
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Data.Foldable
import Data.Either
import Data.String (split)

import Node.Process
import Psc.Ide.Command

import qualified Thermite as T
import qualified Thermite.Action as T

import qualified React as R
import qualified React.DOM as D
import qualified React.DOM.Props as P

data Action = Refresh | LoadModule String | LoadDependency String
type State = {modules :: Array String}
type LoadProps = {}
type LoadEff eff = ( process :: PROCESS | eff )
initialState = {modules: []}

render :: forall eff. T.Render (LoadEff eff) State LoadProps Action
render send state props children = D.div [P.key "load"] [
  D.button
    [P.onClick \_ -> send Refresh]
    [D.text "Loaded Modules"]
  , D.ul' (map (\m -> D.li' [D.text m]) state.modules)
  ]

performAction :: forall eff. T.PerformAction (LoadEff eff) State LoadProps Action
performAction _ Refresh = do
  mods <- liftEff $ list
  case mods of
    Right (Modules ms) -> T.setState {modules: ms}
    Left err -> liftEff (log err)

spec :: forall eff. T.Spec (LoadEff eff) State LoadProps Action
spec = T.simpleSpec initialState performAction render

loadF :: R.ReactElement
loadF = R.createFactory (T.createClass spec) {}
