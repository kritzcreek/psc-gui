module Node.Process where

import Prelude
import Debug.Trace
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Node.Stream
import Node.Encoding
import Psc.Ide.Command
import Data.Maybe
import Data.Argonaut.Encode (EncodeJson, encodeJson)

foreign import data PROCESS :: !

type ChildProcess = {
  stdin  :: forall eff. Writable () eff String,
  stdout :: forall eff. Readable () eff String,
  stderr :: forall eff. Readable () eff String
  }

foreign import spawn :: forall eff.
  String -> Array String -> Eff (process :: PROCESS | eff) ChildProcess

foreign import exec :: forall eff.
  String -> String -> Eff (process :: PROCESS | eff) String

spawnPscIdeServer :: forall eff. String -> Eff (process :: PROCESS | eff) ChildProcess
spawnPscIdeServer directory = do
  process <- spawn "psc-ide-server" ["--debug", "-d", directory]
  setEncoding process.stdout UTF8
  setEncoding process.stderr UTF8
  return process

pscIde q = exec "psc-ide" q

cwd :: forall eff. Eff( process :: PROCESS | eff) (Result Message)
cwd = unwrapResponse <$> (pscIde (show (encodeJson Cwd)))

listLoadedModules :: forall eff. Eff( process :: PROCESS | eff) (Result Modules)
listLoadedModules = unwrapResponse <$> pscIde (show (encodeJson (Ls LoadedModules)))

listImports :: forall eff. String -> Eff( process :: PROCESS | eff) (Result ImportList)
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
