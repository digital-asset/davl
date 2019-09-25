-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- Interact with a DavlLedger, submitting commands and tracking extern transitions.
module Davl.Interact (InteractState(..), makeInteractState, runSubmit) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Davl.DavlLedger (Handle,sendCommand,getTrans)
import Davl.Contracts (DavlContract,DavlTemplate)
import Davl.Domain (Party(..))
import Davl.Local (State,initState,UserCommand,externalizeCommand,applyTrans,applyTransQuiet)
import Davl.Logging (Logger,colourLog)
import DA.Ledger.PastAndFuture (PastAndFuture(..))
import DA.Ledger.Stream (Stream,Closed(EOS,Abnormal,reason),takeStream)
import System.Console.ANSI (Color(..))

data InteractState = InteractState {
    whoami :: Party,
    sv :: MVar State,
    stream :: Stream DavlContract
    }

makeInteractState :: Handle -> Logger -> Party -> IO InteractState
makeInteractState h xlog whoami = do
    sv <- newMVar initState
    let partyLog = colourLog Blue xlog
    stream <- manageUpdates h whoami partyLog sv
    return InteractState{whoami,sv,stream}

sendShowingRejection :: Party -> Handle -> Logger -> DavlTemplate -> IO ()
sendShowingRejection whoami h log cc =
    sendCommand whoami h cc >>= \case
    Nothing -> return ()
    Just rej -> log $ "command rejected by ledger: " <> rej

runSubmit :: Handle -> Logger -> InteractState -> UserCommand -> IO ()
runSubmit h log is uc = do
    --log $ "uc: " <> show uc
    let InteractState{whoami,sv} = is
    s <- readMVar sv
    case externalizeCommand whoami s uc of
        Left reason -> log reason
        Right cc -> sendShowingRejection whoami h log cc

-- Manage updates in response to contracts from the ledger

manageUpdates :: Handle -> Party -> Logger -> MVar State -> IO (Stream DavlContract)
manageUpdates h whoami log sv = do
    PastAndFuture{past,future} <- getTrans whoami h
    log $ "replaying " <> show (length past) <> " transactions"
    modifyMVar_ sv (\s -> return $ foldl (applyTransQuiet whoami) s past)
    _ <- forkIO (updateX whoami log sv future)
    return future

updateX :: Party -> Logger -> MVar State -> Stream DavlContract -> IO ()
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

applyX :: Party -> Logger -> MVar State -> DavlContract -> IO ()
applyX whoami log sv cc = do
    s <- takeMVar sv
    let (s',ans) = applyTrans whoami s cc
    mapM_ (log . show) ans
    putMVar sv s'
