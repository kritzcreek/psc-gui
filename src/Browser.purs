module Browser where

import           Prelude
import           Control.Monad.Eff.Console
import           Data.Maybe.Unsafe
import           Data.Nullable (toMaybe)
import           DOM (DOM())
import           DOM.HTML (window)
import           DOM.HTML.Document (body)
import           DOM.HTML.Types (htmlElementToElement)
import           DOM.HTML.Window (document)
import           DOM.Node.Types (Element())

import           Psc.Gui.Component.Cwd
import           Psc.Gui.Component.Load
import           Psc.Gui.Component.Pursuit
import           Psc.Gui.Component.Import
import           Psc.Gui.Component.Completion
import           Psc.Gui.Component.CardWrapper
import           Psc.Gui.Component.Tabbar
import           Psc.Gui.Component.Types

import Node.Process
import Node.Stream (onData)

import           React
import qualified React.DOM as D
import qualified React.DOM.Props as P

import qualified Signal as S
import qualified Signal.Channel as C

type ApplicationState = {
  activeTab :: Int
}

newState = {activeTab: 0}

step (ChangeTab x) as = as {activeTab=x}

main = do
  body' <- getBody
  ideServer <- spawnPscIdeServer "/home/creek/Documents/psc-gui"
  onData ideServer.stdout log
  channel <- C.channel (ChangeTab 0)
  let actions = C.subscribe channel
  let state = S.foldp step newState actions S.~> (\as ->
    void (render (ui channel as.activeTab) body'))
  S.runSignal state
  where
    ui channel activeTab = D.div' [
      tabbarF {
        tabs: ["Completion", "Modules", "Pursuit", "Imports"],
        activeTab: activeTab,
        channel: channel
        },
      D.div [P.className "ui grid"] [
        D.div [P.className "three column row"] [
          cardWrapper "Current Working Directory" cwdF,
          case activeTab of
            0 -> cardWrapper "Completion" completionF
            1 -> cardWrapper "Loading Modules" loadF
            2 -> cardWrapper "Pursuit" pursuitF
            3 -> cardWrapper "Imports" importF
            _ -> D.text "Wattafak"
        ]
      ]
    ]
    getBody = do
      win <- window
      doc <- document win
      elm <- fromJust <$> toMaybe <$> body doc
      return $ htmlElementToElement elm
