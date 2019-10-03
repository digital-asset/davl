-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Davl.DavlLedger (
    Handle,
    connect,
    sendCommand,
    getTrans
    ) where

import Control.Monad(forM)
import Data.List as List
import System.Random (randomIO)
import qualified Data.Text.Lazy as Text (pack)
import qualified Data.UUID as UUID

import DA.Ledger

import Davl.Domain
import Davl.LedgerTranslation(makeLedgerCommand,extractTransaction)
import Davl.Logging (Logger)
import qualified Davl.HostAndPort as HP

data Handle = Handle {
    hp :: HP.HostAndPort,
    log :: Logger,
    lid :: LedgerId,
    pid :: PackageId
    }

type Rejection = String

run :: HP.HostAndPort -> TimeoutSeconds -> LedgerService a -> IO a
run hp timeout ls = do
    let config = HP.toLedgerConfig hp
    runLedgerService ls timeout config

connect :: HP.HostAndPort -> Logger -> IO Handle
connect hp log = do
    lid <- run hp 5 getLedgerIdentity

    discovery <- run hp 5 $ do
        pids <- listPackages lid
        forM pids $ \pid -> do
            getPackage lid pid >>= \case
                Nothing -> return (pid, False)
                Just package -> do
                    return (pid, containsDavl package)

    case List.filter snd discovery of
        [] -> fail "cant find package containing Davl"
        xs@(_:_:_) -> fail $ "found multiple packages containing Davl: " <> show (map fst xs)
        [(pid,_)] -> return Handle{hp,log,lid,pid}

containsDavl :: Package -> Bool
containsDavl package = "Davl" `isInfixOf` show package -- TODO: be more principled

sendCommand :: Party -> Handle -> DavlCommand -> IO (Maybe Rejection)
sendCommand asParty h@Handle{pid} cc = do
    let com = makeLedgerCommand pid cc
    submitCommand h asParty com >>= \case
        Left rejection -> return $ Just rejection
        Right () -> return Nothing

getTrans :: Party -> Handle -> IO (PastAndFuture DavlEvent)
getTrans party Handle{hp,log,lid} = do
    pf <- run hp 6000 $ getTransactionsPF lid party
    mapListPF (fmap concat . mapM (extractTransaction log)) pf

submitCommand :: Handle -> Party -> Command -> IO (Either String ())
submitCommand Handle{hp,lid} party com = do
    cid <- randomCid
    run hp 5 $
        submit (Commands {lid,wid,aid=myAid,cid,party,leTime,mrTime,coms=[com]})
    where
        wid = Nothing
        leTime = Timestamp 0 0
        mrTime = Timestamp 5 0
        myAid = ApplicationId "davl-console"

randomCid :: IO CommandId
randomCid = do fmap (CommandId . Text.pack . UUID.toString) randomIO
