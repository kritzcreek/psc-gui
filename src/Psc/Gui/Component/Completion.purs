module Psc.Gui.Component.Completion (completionF, CompletionProps(..)) where

import Prelude
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console

import Data.Array
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Tuple (uncurry)
import Psc.Ide.Command
import Psc.Gui.Component.Util
import Node.Process

import qualified Thermite as T
import qualified Thermite.Action as T

import qualified React as R
import qualified React.DOM as D
import qualified React.DOM.Props as P

data Action =
  CompleteSymbol
  | InputChange String
  | FilterChange Int Filter
  | RemoveFilter Int
  | AddFilter

type State = {
  input      :: String,
  filterType :: String,
  filters    :: Array Filter,
  matcher    :: Maybe Matcher
}

type CompletionProps = {}

type CompletionEff eff = ( console :: CONSOLE | eff )

initialState = {
  input: "",
  filters: [],
  filterType: "ExactFilter",
  matcher: Nothing
}

renderFilter ::  _ -> Int -> Filter -> R.ReactElement
renderFilter send filterId (ExactFilter s) =
  D.div [P.key (show filterId)] [
    sinput [P.onChange \ev ->
      send (FilterChange filterId (ExactFilter (unsafeTargetValue ev))),
      P.value s
      ],
    sbutton "red" [P.onClick (\_ ->
      send (RemoveFilter filterId))]
      [D.text "Remove"]
  ]

render :: forall eff. T.Render (CompletionEff eff) State CompletionProps Action
render send state props children =
  D.div' [
    sinput [P.onChange \ev -> send (InputChange (unsafeTargetValue ev))],
    sbutton "green" [P.onClick \_ -> send CompleteSymbol] [D.text "Complete"],
    sbutton "" [P.onClick \_ -> send AddFilter] [D.text "Add Filter"],
    D.div' (zipWithEnumerated (renderFilter send) state.filters)
  ]

performAction :: forall eff. T.PerformAction (CompletionEff eff) State CompletionProps Action
performAction _ CompleteSymbol = return unit
performAction _ (InputChange i) = T.modifyState \s -> s{input=i}
performAction _ AddFilter = T.modifyState \s ->
  s{filters = s.filters `snoc` (ExactFilter "")}
performAction _ (RemoveFilter filterId) = do
  s <- T.getState
  T.setState (s {filters = fromJust (deleteAt filterId s.filters)})
performAction _ (FilterChange filterId newFilter) = T.modifyState \s ->
  s{filters = fromJust (modifyAt filterId (const newFilter) s.filters)}

spec :: forall eff. T.Spec (CompletionEff eff) State CompletionProps Action
spec = T.simpleSpec initialState performAction render

completionF :: R.ReactElement
completionF = R.createFactory (T.createClass spec) {}
