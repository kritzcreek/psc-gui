module Psc.Gui.Component.Import (importF) where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Data.Array (head)
import Data.Either
import Data.Maybe (fromMaybe)
import Psc.Ide.Command
import Psc.Gui.Component.Util
import Node.Process
import Electron.Dialog

import qualified Thermite as T
import qualified Thermite.Action as T

import qualified React as R
import qualified React.DOM as D
import qualified React.DOM.Props as P

data Action = Submit | Dialog
type State = {
  input   :: String,
  imports :: String -- TODO: make this a real type
  }
initialState = {input: "", imports: ""}
type Props = Unit
type ImportEff eff = (process :: PROCESS | eff)

render :: forall eff. T.Render (ImportEff eff) State Props Action
render send s _ children = D.div [P.key "import"] [
  D.p' [D.text s.input],
  sbutton "blue"
    [P.onClick \_ -> send Submit]
    [D.text "Refresh"],
  sbutton "red"
    [P.onClick \_ -> send Dialog]
    [D.text "Choose File"],
  D.p' [D.text s.imports]
  ]

performAction :: forall eff. T.PerformAction (ImportEff eff) State Props Action
performAction _ Submit = do
  s <- T.getState
  dir <- liftEff (listImports s.input)
  case dir of
    Right (Message imports) -> T.modifyState (_ {imports=imports})
    Left err -> do
      liftEff $ log err
      T.modifyState (_ {imports=err})
performAction _ Dialog = do
  paths <- liftEff $ showOpenDialog defaultOpts
  T.modifyState (_ {input=fromMaybe "" (head (fromMaybe [""] paths))})

spec :: forall eff. T.Spec (ImportEff eff) State Props Action
spec = T.simpleSpec initialState performAction render

importF :: R.ReactElement
importF = R.createFactory (T.createClass spec) unit
