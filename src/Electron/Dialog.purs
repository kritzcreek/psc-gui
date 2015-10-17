module Electron.Dialog where

import Prelude
import Control.Monad.Eff
import Data.Maybe

foreign import data DIALOG :: !

type DialogFilter = {
  name :: String,
  extensions :: String
  }

data DialogProperty = OpenFile
                    | OpenDirectory
                    | MultiSelections
                    | CreateDirectory

instance showDialogProperty :: Show DialogProperty where
  show OpenFile = "openFile"
  show OpenDirectory = "openDirectory"
  show MultiSelections = "multiSelections"
  show CreateDirectory = "createDirectory"

type DialogOptions = {
  title       :: String,
  defaultPath :: String,
  filters     :: Array DialogFilter,
  properties  :: Array DialogProperty
  }

defaultOpts = {title: "default", defaultPath: "", filters: [], properties: []}

foreign import showOpenDialogImpl ::
  forall eff a.
  DialogOptions ->
  (a -> Maybe a) ->
  Maybe a ->
  Eff (dialog :: DIALOG | eff) (Maybe (Array String))

showOpenDialog :: forall eff. DialogOptions -> Eff (dialog :: DIALOG | eff) (Maybe (Array String))
showOpenDialog opts = showOpenDialogImpl opts Just Nothing
