module Node.Process where

import Prelude
import Control.Monad.Eff
import Psc.Ide.Command
import Data.Argonaut.Encode (EncodeJson, encodeJson)

foreign import data PROCESS :: !

foreign import exec :: forall eff.
  String -> String -> Eff (process :: PROCESS | eff) String

pscIde q = exec "psc-ide" q

cwd :: forall eff. Eff( process :: PROCESS | eff) (Result Message)
cwd = unwrapResponse <$> (pscIde (show (encodeJson Cwd)))

list :: forall eff. Eff( process :: PROCESS | eff) (Result Modules)
list = unwrapResponse <$> pscIde (show (encodeJson Ls))

quit :: forall eff. Eff( process :: PROCESS | eff) (Result Message)
quit = unwrapResponse <$> pscIde (show (encodeJson Quit))

pursuitCompletion :: forall eff. String -> Eff( process :: PROCESS | eff) (Result PursuitCompletion)
pursuitCompletion q = unwrapResponse <$> pscIde (show (encodeJson (Pursuit Ident q)))
