module Psc.Gui.Component.Pursuit (pursuitF, PursuitProps(..)) where

import Prelude
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Data.Either

import Node.Process
import Psc.Ide.Command
import Psc.Gui.Component.Util

import qualified Thermite as T
import qualified Thermite.Action as T

import qualified React as R
import qualified React.DOM as D
import qualified React.DOM.Props as P

data Action = SearchCompletion | SearchModule | InputChange String
type State = {
  packages :: Array String,
  completions :: Array PursuitCompletion,
  input :: String
}
type PursuitProps = {}
type PursuitEff eff = ( process :: PROCESS, console :: CONSOLE | eff )
initialState = {packages: [], completions: [], input: "Query"}

pursuitCompletionCard :: PursuitCompletion -> R.ReactElement
pursuitCompletionCard (PursuitCompletion pc)= D.li' [
  D.p' [D.text "Package: ", D.text (pc.package)],
  D.p' [D.text "Module: ", D.text (pc.module')],
  D.p' [D.text "Identifier: ", D.text (pc.identifier)],
  D.p' [D.text "Type: ", D.text (pc.type')]
]

render :: forall eff. T.Render (PursuitEff eff) State PursuitProps Action
render send state props children =
  D.div' [
  sinput
    [P.onChange \e -> send (InputChange (unsafeTargetValue e))
    , P.value state.input
    ],
  sbutton "green"
    [P.onClick \_ -> send SearchCompletion]
    [D.text "Search completions"],
  D.ul' (map pursuitCompletionCard state.completions)
  ]


performAction :: forall eff. T.PerformAction (PursuitEff eff) State PursuitProps Action
performAction _ SearchCompletion = do
  s <- T.getState
  res <- liftEff (pursuitCompletion s.input)
  case res of
    Right cs -> T.modifyState (\s -> s{completions=cs})
    Left err -> liftEff (log err)
performAction _ SearchModule = return unit
performAction _ (InputChange i) = T.modifyState (\s -> s {input=i})


spec :: forall eff. T.Spec (PursuitEff eff) State PursuitProps Action
spec = T.simpleSpec initialState performAction render

pursuitF :: R.ReactElement
pursuitF = R.createFactory (T.createClass spec) {}
