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
import Davl.Local as Local (State,UserCommand(Give),history)
import Davl.Logging (colourLog,plainLog,colourWrap)

replyLog :: String -> IO ()
replyLog = colourLog Cyan plainLog

interactiveMain :: Party -> IO ()
interactiveMain party = HL.runInputT HL.defaultSettings $ do
    xlog <- HL.getExternalPrint
    let errLog = colourLog Red xlog
    h <- lift (connect errLog)
    ps <- lift $ makeInteractState h xlog party
    lift $ replyLog "type :help to see available commands"
    readLoop h ps

-- readLoop

prompt :: InteractState -> String
prompt InteractState{whoami,talking} =
    colourWrap Green (show whoami <> "> " <> target)
    where target = case talking of
              Nothing -> ""
              Just to -> "(" ++ show to ++ ") "

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

data ChangeContext
    = BeginShout
    | BeginTalk Party
    deriving (Show)

data Command
    = NoCommand
    | Submit UserCommand
    | Query Query
    | ChangeContext ChangeContext
    deriving (Show)

-- parse console input line

parseLine :: String -> Command
parseLine line = case words line of
    [] -> Query History
    [":help"] ->
        Query Help
    ["h"] ->
        Query History
    [":history"] ->
        Query History
    ["!"] ->
        ChangeContext BeginShout
    ['!':who] ->
        ChangeContext (BeginTalk (party who))
    ["!",who] ->
        ChangeContext (BeginTalk (party who))
    ["."] ->
        Submit $ Local.Give
    _ ->
        NoCommand

  where
      party = Party . Text.pack


helpText :: String
helpText = unlines [
    "?            List everyone I know; they hear my shouts",
    "!<Name>      Switch to a private chat with <Name> (who must be known)",
    "!            Switch to shouting",
    ":help        Display this help text",
    ":history     Show the history of messages and links",
    ":link <Name> Link with <Name>, connecting our known/shout networks",
    "h            Alias for :history"
    ]

-- run the parsed command

runCommand :: Handle -> InteractState -> Command -> IO InteractState
runCommand h is = \case
    NoCommand -> return is
    Submit uc -> do
        runSubmit h replyLog is uc
        return is
    Query lq -> do
        let InteractState{sv} = is
        s <- readMVar sv
        runLocalQuery s lq
        return is
    ChangeContext change ->
        runChangeContext is change

runLocalQuery :: State -> Query -> IO ()
runLocalQuery s = \case
    Help -> replyLog helpText
    History -> replyLog (unlines $ map show (Local.history s))

runChangeContext :: InteractState -> ChangeContext -> IO InteractState
runChangeContext is = \case
    BeginShout -> return $ is { talking = Nothing }
    BeginTalk to -> return $ is { talking = Just to }
        {-
        let InteractState{sv} = is
        s <- readMVar sv
        if True -- to `elem` Local.known s
            then return $ is { talking = Just to }
            else do replyLog "you don't know this person; type ? to see who you know"
                    return is
-}
