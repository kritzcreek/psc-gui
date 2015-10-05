module Psc.Gui.Component.Cwd (cwdF) where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Data.Either
import Psc.Ide.Command
import Psc.Gui.Component.Util
import Node.Process

import qualified Thermite as T
import qualified Thermite.Action as T

import qualified React as R
import qualified React.DOM as D
import qualified React.DOM.Props as P

data Action = Refresh
type State = {directory :: String}
initialState = {directory: ""}
type Props = Unit
type CwdEff eff = (process :: PROCESS | eff)

render :: forall eff. T.Render (CwdEff eff) State Props Action
render send s _ children = D.div [P.key "cwd"] [
  sbutton "blue"
    [P.onClick \_ -> send Refresh]
    [D.text "Refresh"],
  D.p' [D.text s.directory]
  ]

performAction :: forall eff. T.PerformAction (CwdEff eff) State Props Action
performAction _ Refresh = do
  dir <- liftEff cwd
  case dir of
    Right (Message dir) -> T.modifyState (const {directory: dir})
    Left err -> do
      liftEff $ log err
      T.setState {directory: err}

spec :: forall eff. T.Spec (CwdEff eff) State Props Action
spec = T.componentWillMount Refresh $
  T.simpleSpec initialState performAction render

cwdF :: R.ReactElement
cwdF = R.createFactory (T.createClass spec) unit
