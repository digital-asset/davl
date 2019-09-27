-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Davl.UI (interactiveMain) where

import Control.Concurrent.MVar
import Control.Monad.Trans.Class (lift)
import Data.Text.Lazy as Text (pack)
import System.Console.ANSI (Color(..))
import qualified System.Console.Haskeline as HL

import Davl.DavlLedger (Handle,connect)
import Davl.Domain (Party(..))
import Davl.Interact (InteractState(..),makeInteractState,runSubmit)
import Davl.Local as Local (State,UserCommand(..),history)
import Davl.Logging (colourLog,plainLog,colourWrap)

replyLog :: String -> IO ()
replyLog = colourLog Cyan plainLog

interactiveMain :: Party -> IO ()
interactiveMain party = HL.runInputT HL.defaultSettings $ do
    xlog <- HL.getExternalPrint
    let errLog = colourLog Red xlog
    h <- lift (connect errLog)
    ps <- lift $ makeInteractState h xlog party
    lift $ replyLog "type \"help\" to see available commands"
    readLoop h ps

-- readLoop

prompt :: InteractState -> String
prompt InteractState{whoami} =
    colourWrap Green (show whoami <> "> ")

readLoop :: Handle -> InteractState -> HL.InputT IO ()
readLoop h is = do
    lineOpt <- HL.getInputLine (prompt is)
    case lineOpt of
      Nothing -> return ()
      Just line -> do
          is' <- lift $ runCommand h is $ parseLine line
          readLoop h is'

-- console commands

data Query
    = Help
    | History
    deriving (Show)

data Command
    = Unexpected [String]
    | Submit UserCommand
    | Query Query
    deriving (Show)

-- parse console input line

parseLine :: String -> Command
parseLine line = case words line of
    [] -> Query History
    ["help"] -> Query Help
    ["give",guy] -> Submit (Local.GiveTo (party guy))
    ["claim",guy] -> Submit (Local.ClaimFrom (party guy))
    words ->
        Unexpected words

  where
      party = Party . Text.pack


helpText :: String
helpText = unlines
    [ "give <Name>    Send a Gift to <Name>"
    , "claim <Name>   Claim a Gift from <Name>"
    , "help           Display this help text"
    , "<return>       Show the history of sent and received Gifts"
    ]

-- run the parsed command

runCommand :: Handle -> InteractState -> Command -> IO InteractState
runCommand h is = \case
    Unexpected words -> do
        replyLog $ "Parse error: " <> unwords words
        return is
    Submit uc -> do
        runSubmit h replyLog is uc
        return is
    Query lq -> do
        let InteractState{sv} = is
        s <- readMVar sv
        runLocalQuery s lq
        return is

runLocalQuery :: State -> Query -> IO ()
runLocalQuery s = \case
    Help -> replyLog helpText
    History -> replyLog (unlines $ map show (Local.history s))
