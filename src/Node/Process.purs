module Node.Process where

import Prelude
import Control.Monad.Eff
import Psc.Ide.Command
import Data.Maybe
import Data.Argonaut.Encode (EncodeJson, encodeJson)

foreign import data PROCESS :: !

foreign import exec :: forall eff.
  String -> String -> Eff (process :: PROCESS | eff) String

pscIde q = exec "psc-ide" q

cwd :: forall eff. Eff( process :: PROCESS | eff) (Result Message)
cwd = unwrapResponse <$> (pscIde (show (encodeJson Cwd)))

listLoadedModules :: forall eff. Eff( process :: PROCESS | eff) (Result Modules)
listLoadedModules = unwrapResponse <$> pscIde (show (encodeJson (Ls LoadedModules)))

listImports :: forall eff. String -> Eff( process :: PROCESS | eff) (Result Message)
listImports fp = unwrapResponse <$> pscIde (show (encodeJson (Ls (Imports fp))))

load :: forall eff.
  Array String ->
  Array String ->
  Eff( process :: PROCESS | eff) (Result Message)
load ms ds = unwrapResponse <$> pscIde (show (encodeJson (Load ms ds)))

quit :: forall eff. Eff( process :: PROCESS | eff) (Result Message)
quit = unwrapResponse <$> pscIde (show (encodeJson Quit))

pursuitCompletion :: forall eff. String -> Eff( process :: PROCESS | eff) (Result (Array PursuitCompletion))
pursuitCompletion q = unwrapResponse <$> pscIde (show (encodeJson (Pursuit Ident q)))

complete :: forall eff.
  Array Filter ->
  Maybe Matcher ->
  Eff ( process :: PROCESS | eff) (Result (Array Completion))
complete fs m = unwrapResponse <$> pscIde (show (encodeJson (Complete fs m)))
