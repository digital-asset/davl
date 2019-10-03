-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Davl.UI (interactiveMain) where

import Control.Concurrent.MVar
import Control.Monad.Trans.Class (lift)
import Data.Text.Lazy as Text (pack)
import System.Console.ANSI (Color(..))
import qualified System.Console.Haskeline as HL

import Davl.HostAndPort(HostAndPort)
import Davl.DavlLedger (Handle,connect)
import Davl.Domain
import Davl.Logging (colourLog,plainLog,colourWrap)

import qualified Davl.Interact as Interact
import qualified Davl.ContractStore as CS
import qualified Davl.Aggregation as AG
import qualified Davl.ListRequests as LR
import qualified Davl.ListDenials as LD
import qualified Davl.ListVacations as LV

replyLog :: String -> IO ()
replyLog = colourLog Cyan plainLog

interactiveMain :: HostAndPort -> Party -> IO ()
interactiveMain hp party = HL.runInputT HL.defaultSettings $ do
    xlog <- HL.getExternalPrint
    let errLog = colourLog Red xlog
    h <- lift (connect hp errLog)
    ps <- lift $ Interact.makeState h xlog party
    lift $ replyLog "type \"help\" to see available commands"
    readLoop h ps

-- readLoop

prompt :: Interact.State -> String
prompt Interact.State{whoami} =
    colourWrap Green (show whoami <> "> ")

readLoop :: Handle -> Interact.State -> HL.InputT IO ()
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
    | ShowHistory
    | ShowSummary
    | ShowPending
    | ShowDenials
    | ShowVacations
    deriving (Show)

data Command
    = Unexpected [String]
    | Submit Interact.Command
    | Query Query
    deriving (Show)

-- parse console input line

parseLine :: String -> Command
parseLine line = case words line of
    ["help"] -> Query Help

    ["give",guy] -> Submit (Interact.GiveTo (party guy))
    ["claim",guy] -> Submit (Interact.ClaimFrom (party guy))
    ["request",n] -> Submit (Interact.RequestDate (Date {daysSinceEpoch = read n}))
    ["deny",n,why] -> Submit (Interact.DenyRequestNumber (read n) why)
    ["approve",n] -> Submit (Interact.ApproveRequestNumber (read n))

    ["history"] -> Query ShowHistory
    ["h"] -> Query ShowHistory

    ["summary"] -> Query ShowSummary
    ["s"] -> Query ShowSummary
    [] -> Query ShowSummary

    ["pending"] -> Query ShowPending
    ["p"] -> Query ShowPending

    ["denials"] -> Query ShowDenials
    ["d"] -> Query ShowDenials

    ["vacations"] -> Query ShowVacations
    ["v"] -> Query ShowVacations

    words ->
        Unexpected words

  where
      party = Party . Text.pack


helpText :: String
helpText = unlines
    [ "give <Name>     Send a Gift to <Name>"
    , "claim <Name>    Claim a Gift from <Name>"
    , "request <N>     Request a holiday <N> days from epoch(!), using any allocation day"
    , "deny <N>        Deny request #N (as listed by pending)"

    , "history/h       Show the history of contract creations/archivals"
    , "summary/s       Show summary of holiday status, as boss/employee"
    , "pending/p       Show pending requests for holiday, as boss/employee"
    , "denials/d       Show denials for holiday requests, as employee"
    , "vacations/v     Show booked vacation, as boss/employee"

    , "<return>        Alias for summary"
    , "help            Display this help text"
    ]

-- run the parsed command

runCommand :: Handle -> Interact.State -> Command -> IO Interact.State
runCommand h is = \case
    Unexpected words -> do
        replyLog $ "Parse error: " <> unwords words
        return is
    Submit uc -> do
        Interact.runSubmit h replyLog is uc
        return is
    Query lq -> do
        let Interact.State{sv,whoami} = is
        s <- readMVar sv
        runLocalQuery whoami s lq
        return is

runLocalQuery :: Party -> CS.State -> Query -> IO ()
runLocalQuery whoami s = \case
    Help -> replyLog helpText
    ShowHistory -> replyLog (unlines $ map show (CS.history s))
    ShowSummary -> do
        replyLog (show (AG.summaryAsBoss whoami s))
        replyLog (show (AG.summaryAsEmployee whoami s))
    ShowPending -> do
        replyLog (show (LR.listAsBoss whoami s))
        replyLog (show (LR.listAsEmployee whoami s))
    ShowDenials -> do
        replyLog (show (LD.listAsEmployee whoami s))
    ShowVacations -> do
        replyLog (show (LV.listAsBoss whoami s))
        replyLog (show (LV.listAsEmployee whoami s))
