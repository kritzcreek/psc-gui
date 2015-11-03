module Psc.Gui.Component.Pursuit (pursuitF, PursuitProps(..)) where

import Prelude
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Data.Either

import Node.Process
import Psc.Ide.Command
import Psc.Gui.Component.Util

import qualified Thermite as T

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

render :: T.Render State PursuitProps Action
render send _ state children = [ D.div' [
  sinput
    [P.onChange \e -> send (InputChange (unsafeTargetValue e))
    , P.value state.input
    ],
  sbutton "green"
    [P.onClick \_ -> send SearchCompletion]
    [D.text "Search completions"],
  D.ul' (map pursuitCompletionCard state.completions)
  ] ]


performAction :: forall eff. T.PerformAction (PursuitEff eff) State PursuitProps Action
performAction SearchCompletion _ s k = do
  res <- pursuitCompletion s.input
  case res of
    Right cs -> k (s { completions=cs })
    Left err -> log err
performAction SearchModule _ s k = return unit
performAction (InputChange i) _ s k = k (s { input=i })


spec :: forall eff. T.Spec (PursuitEff eff) State PursuitProps Action
spec = T.simpleSpec performAction render

pursuitF :: R.ReactElement
pursuitF = R.createFactory (T.createClass spec initialState) {}
