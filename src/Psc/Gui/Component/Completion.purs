module Psc.Gui.Component.Completion (completionF, CompletionProps(..)) where

import Prelude
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console

import Data.Array
import Data.Either
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
  | FilterTypeChange String
  | FilterChange Int Filter
  | RemoveFilter Int
  | AddFilter

type State = {
  input       :: String,
  filterType  :: String,
  filters     :: Array Filter,
  matcher     :: Maybe Matcher,
  completions :: Array Completion
}

type CompletionProps = {}

type CompletionEff eff = ( console :: CONSOLE | eff )

initialState = {
  input: "",
  filters: [],
  filterType: "ExactFilter",
  matcher: Nothing,
  completions: []
}

renderFilter ::  _ -> Int -> Filter -> R.ReactElement
renderFilter send filterId filter' =
  D.div [P.key (show filterId)] [
    D.p' [D.text (showFilterType filter')],
    sinput [P.onChange \ev ->
      send (FilterChange filterId (constructFilter filter' (unsafeTargetValue ev))),
      P.value (showFilter filter')
      ],
    sbutton "red" [P.onClick (\_ ->
      send (RemoveFilter filterId))]
      [D.text "Remove"]
  ]
  where
    showFilterType (ExactFilter _) = "Exact Filter"
    showFilterType (PrefixFilter _) = "Prefix Filter"
    showFilterType (ModuleFilter _) = "Module Filter"
    showFilterType (DependencyFilter _) = "Dependency Filter"

    showFilter (ExactFilter s) = s
    showFilter (PrefixFilter s) = s
    showFilter (ModuleFilter s) = fromJust (head s)
    showFilter (DependencyFilter s) = fromJust (head s)

    constructFilter (ExactFilter _) new = ExactFilter new
    constructFilter (PrefixFilter _) new = PrefixFilter new
    constructFilter (ModuleFilter _) new = ModuleFilter [new]
    constructFilter (DependencyFilter _) new = DependencyFilter [new]

renderCompletion :: Completion -> R.ReactElement
renderCompletion (Completion c) =
  D.div' [
    D.p' [D.text $ "Identifier: " ++ c.identifier],
    D.p' [D.text $ "Type: " ++ c.type'],
    D.p' [D.text $ "Module: " ++ c.module']
  ]

render :: forall eff. T.Render (CompletionEff eff) State CompletionProps Action
render send state props children =
  D.div' [
    D.select [P.onChange \ev -> send (FilterTypeChange (unsafeTargetValue ev))]
      (map (\x -> D.option' [D.text x]) ["ExactFilter", "PrefixFilter",   "ModuleFilter", "DependencyFilter"]),
    sinput [P.onChange \ev -> send (InputChange (unsafeTargetValue ev))],
    sbutton "green" [P.onClick \_ -> send CompleteSymbol] [D.text "Complete"],
    sbutton "" [P.onClick \_ -> send AddFilter] [D.text "Add Filter"],
    D.div' (zipWithEnumerated (renderFilter send) state.filters),
    D.div' (map renderCompletion state.completions)
  ]

performAction :: forall eff. T.PerformAction (CompletionEff eff) State CompletionProps Action
performAction _ CompleteSymbol = do
  s <- T.getState
  r <- liftEff $ complete s.filters if s.input /= ""
                                    then Just (Flex s.input)
                                    else Nothing
  case r of
    Left err -> liftEff $ log err
    Right r  -> T.modifyState (_ {completions=r})
performAction _ (InputChange i) = T.modifyState \s -> s{input=i}
performAction _ AddFilter =
  let construct "ExactFilter" = ExactFilter ""
      construct "PrefixFilter" = PrefixFilter ""
      construct "ModuleFilter" = ModuleFilter [""]
      construct "DependencyFilter" = DependencyFilter [""]
  in T.modifyState \s ->
    s{filters = s.filters `snoc` (construct s.filterType)}
performAction _ (RemoveFilter filterId) = do
  T.modifyState (\s -> s {filters = fromJust (deleteAt filterId s.filters)})
performAction _ (FilterChange filterId newFilter) = T.modifyState \s ->
  s{filters = fromJust (modifyAt filterId (const newFilter) s.filters)}
performAction _ (FilterTypeChange s) = T.modifyState (_ {filterType=s})

spec :: forall eff. T.Spec (CompletionEff eff) State CompletionProps Action
spec = T.simpleSpec initialState performAction render

completionF :: R.ReactElement
completionF = R.createFactory (T.createClass spec) {}
