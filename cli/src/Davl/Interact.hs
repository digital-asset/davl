-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- Interact with a DavlLedger, submitting commands and tracking extern transitions.
module Davl.Interact (
    State(..), makeState,
    Command(..), runSubmit
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar(MVar,newMVar,readMVar,modifyMVar_,takeMVar,putMVar)
import Data.Maybe(listToMaybe)
import System.Console.ANSI (Color(..))

import DA.Ledger.PastAndFuture (PastAndFuture(..))
import DA.Ledger.Stream (Stream,Closed(EOS,Abnormal,reason),takeStream)

import Davl.DavlLedger (Handle,sendCommand,getTrans)
import Davl.Domain
import Davl.Logging (Logger,colourLog)
import qualified Davl.ContractStore as CS

data State = State {
    whoami :: Party,
    sv :: MVar CS.State,
    stream :: Stream DavlEvent
    }

makeState :: Handle -> Logger -> Party -> IO State
makeState h xlog whoami = do
    sv <- newMVar CS.initState
    let partyLog = colourLog Blue xlog
    stream <- manageUpdates h whoami partyLog sv
    return State{whoami,sv,stream}

sendShowingRejection :: Party -> Handle -> Logger -> DavlCommand -> IO ()
sendShowingRejection whoami h log cc =
    sendCommand whoami h cc >>= \case
    Nothing -> return ()
    Just rej -> log $ "command rejected by ledger: " <> rej

runSubmit :: Handle -> Logger -> State -> Command -> IO ()
runSubmit h log is uc = do
    --log $ "uc: " <> show uc
    let State{whoami,sv} = is
    s <- readMVar sv
    case externalizeCommand whoami s uc of
        Left reason -> log reason
        Right cc -> sendShowingRejection whoami h log cc

-- user commands, to be interpreted w.r.t the local state

data Command
    = GiveTo Party
    | ClaimFrom Party
    | RequestDate Date
    | DenyRequestNumber Int String
    deriving Show

externalizeCommand :: Party -> CS.State -> Command -> Either String DavlCommand
externalizeCommand whoami state = \case
    GiveTo employee ->
        return $ GiveGift $ Gift { allocation = Holiday { boss = whoami, employee } }
    ClaimFrom boss -> do
        case findGiftToMeFrom state whoami boss of
            Nothing -> Left $ "no gift found from: " <> show boss
            Just id -> return $ ClaimGift id
    RequestDate date -> do
        case findAnyAllocation state whoami of
            Nothing -> Left $ "no holiday allocation available"
            Just (allocationId,Holiday{boss}) ->
                return $ RequestHoliday $ Request
                { employee = whoami
                , boss
                , allocationId
                , date
                }
    DenyRequestNumber num why -> do
        case findRequestAsBoss state whoami of
            [] -> Left $ "there are no pending requests for you to deal with"
            requests ->
                if num < 1 || num > length requests
                then Left $ unwords
                     [ "you have pending requests numbered 1 to",
                       show (length requests),
                       "but you typed:",
                       show num
                     ]
                else do
                    let allocationId = requests !! (num-1) -- indexing from 1
                    return $ DenyRequest allocationId why

findRequestAsBoss :: CS.State -> Party -> [DavlContractId]
findRequestAsBoss state whoami = do
    (id,Request{boss}) <- activeRequests state
    if boss==whoami then return id else []

findAnyAllocation :: CS.State -> Party -> Maybe (DavlContractId,Holiday)
findAnyAllocation state whoami = listToMaybe $ do
    (id,hol@Holiday{employee}) <- activeHolidays state
    if employee==whoami then return (id,hol) else []

activeHolidays :: CS.State ->  [(DavlContractId,Holiday)]
activeHolidays state = do
    (id,THoliday holiday) <- CS.activeContracts state
    return (id,holiday)

findGiftToMeFrom :: CS.State -> Party -> Party -> Maybe DavlContractId
findGiftToMeFrom state whoami bossK = listToMaybe $ do
    (id,Gift{allocation=Holiday{employee,boss}}) <- activeGifts state
    if boss==bossK && employee==whoami then return id else []

activeGifts :: CS.State ->  [(DavlContractId,Gift)]
activeGifts state = do
    (id,TGift gift) <- CS.activeContracts state
    return (id,gift)

activeRequests :: CS.State ->  [(DavlContractId,Request)]
activeRequests state = do
    (id,TRequest request) <- CS.activeContracts state
    return (id,request)


-- Manage updates in response to contracts from the ledgerS.

manageUpdates :: Handle -> Party -> Logger -> MVar CS.State -> IO (Stream DavlEvent)
manageUpdates h whoami log sv = do
    PastAndFuture{past,future} <- getTrans whoami h
    log $ "replaying " <> show (length past) <> " transactions"
    modifyMVar_ sv (\s -> return $ foldl CS.applyTrans s past)
    _ <- forkIO (updateX log sv future)
    return future

updateX :: Logger -> MVar CS.State -> Stream DavlEvent -> IO ()
updateX log sv stream = loop
  where
    loop = do
        takeStream stream >>= \case
            Left EOS -> do
                log "transaction stream has reached EOS"
            Left Abnormal{reason} -> do
                log $ "transaction stream is closed: " <> reason
            Right cc -> do
                applyX log sv cc
                loop

applyX :: Logger -> MVar CS.State -> DavlEvent -> IO ()
applyX log sv event = do
    s <- takeMVar sv
    let s' = CS.applyTrans s event
    log $ show event
    putMVar sv s'
