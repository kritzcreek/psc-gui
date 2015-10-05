module Browser where

import           Prelude
import           Control.Monad.Eff.Console
import           Data.Either
import           Data.Maybe.Unsafe
import           Data.Nullable (toMaybe)
import           PscIde.Command
import           Node.Process
import           DOM (DOM())
import           DOM.HTML (window)
import           DOM.HTML.Document (body)
import           DOM.HTML.Types (htmlElementToElement)
import           DOM.HTML.Window (document)
import           DOM.Node.Types (Element())

import React
import qualified React.DOM as D
import qualified React.DOM.Props as P

comp :: String -> _ -> ReactElement
comp s ctx = D.div' [
  D.button
    [P.onClick \_ -> do
      (Right (Message d)) <- cwd
      writeState ctx d]
    [D.text "CWD"]
  , D.text s
  ]
  where logMessage (Message m) = log m

testComponent :: ReactClass String
testComponent = createClass $ spec "" \ctx -> do
  s <- readState ctx
  return (comp s ctx)

main = do
  body' <- getBody
  render ui body'
  where
    ui = D.div' [ createFactory testComponent "" ]

    getBody = do
      win <- window
      doc <- document win
      elm <- fromJust <$> toMaybe <$> body doc
      return $ htmlElementToElement elm
