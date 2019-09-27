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
    deriving Show

externalizeCommand :: Party -> CS.State -> Command -> Either String DavlCommand
externalizeCommand whoami state = \case
    GiveTo employee ->
        return $ GiveGift $ Gift { allocation = Holiday { boss = whoami, employee } }
    ClaimFrom boss -> do
        case findGiftToMeFrom state whoami boss of
            Nothing -> Left $ "no gift found from: " <> show boss
            Just id -> return $ ClaimGift id


findGiftToMeFrom :: CS.State ->  Party -> Party -> Maybe DavlContractId
findGiftToMeFrom state whoami bossK = listToMaybe $ do
    (id,Gift{allocation=Holiday{employee,boss}}) <- activeGifts state
    if boss==bossK && employee==whoami then return id else []

activeGifts :: CS.State ->  [(DavlContractId,Gift)]
activeGifts state = do
    (id,TGift gift) <- CS.activeContracts state
    return (id,gift)


-- Manage updates in response to contracts from the ledgerS.

manageUpdates :: Handle -> Party -> Logger -> MVar CS.State -> IO (Stream DavlEvent)
manageUpdates h whoami log sv = do
    PastAndFuture{past,future} <- getTrans whoami h
    log $ "replaying " <> show (length past) <> " transactions"
    modifyMVar_ sv (\s -> return $ foldl (CS.applyTrans whoami) s past)
    _ <- forkIO (updateX whoami log sv future)
    return future

updateX :: Party -> Logger -> MVar CS.State -> Stream DavlEvent -> IO ()
updateX whoami log sv stream = loop
  where
    loop = do
        takeStream stream >>= \case
            Left EOS -> do
                log "transaction stream has reached EOS"
            Left Abnormal{reason} -> do
                log $ "transaction stream is closed: " <> reason
            Right cc -> do
                applyX whoami log sv cc
                loop

applyX :: Party -> Logger -> MVar CS.State -> DavlEvent -> IO ()
applyX whoami log sv event = do
    s <- takeMVar sv
    let s' = CS.applyTrans whoami s event
    log $ show event
    putMVar sv s'
