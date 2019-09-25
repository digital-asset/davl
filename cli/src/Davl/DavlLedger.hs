-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0


-- Abstraction for a Ledger which is hosting the Davl domain model.
-- This is basically unchanged from the Nim example
module Davl.DavlLedger (Handle, connect, sendCommand, getTrans) where

import Control.Monad(forM)
import DA.Ledger as Ledger
import Davl.Contracts (DavlContract,DavlTemplate,extractTransaction,makeLedgerCommand)
import Davl.Logging (Logger)
import Data.List as List
import Data.Maybe (maybeToList)
import System.Random (randomIO)
import qualified Data.Text.Lazy as Text (pack)
import qualified Data.UUID as UUID

data Handle = Handle {
    log :: Logger,
    lid :: LedgerId,
    pid :: PackageId
    }

type Rejection = String

port :: Port
port = 6865 -- port on which we expect to find a ledger. should be a command line option

run :: TimeoutSeconds -> LedgerService a -> IO a
run timeout ls  = runLedgerService ls timeout (configOfPort port)

connect :: Logger -> IO Handle
connect log = do
    lid <- run 5 getLedgerIdentity

    discovery <- run 5 $ do
        pids <- listPackages lid
        forM pids $ \pid -> do
            getPackage lid pid >>= \case
                Nothing -> return (pid, False)
                Just package -> do
                    return (pid, containsDavl package)

    case List.filter snd discovery of
        [] -> fail "cant find package containing Davl"
        xs@(_:_:_) -> fail $ "found multiple packages containing Davl: " <> show (map fst xs)
        [(pid,_)] -> return Handle{log,lid,pid}

containsDavl :: Package -> Bool
containsDavl package = "Davl" `isInfixOf` show package -- TODO: be more principled

sendCommand :: Party -> Handle -> DavlTemplate -> IO (Maybe Rejection)
sendCommand asParty h@Handle{pid} cc = do
    let com = makeLedgerCommand pid cc
    submitCommand h asParty com >>= \case
        Left rejection -> return $ Just rejection
        Right () -> return Nothing

getTrans :: Party -> Handle -> IO (PastAndFuture DavlContract)
getTrans party Handle{log,lid} = do
    pf <- run 6000 $ getTransactionsPF lid party
    mapListPF (fmap concat . mapM (fmap maybeToList . extractTransaction log)) pf

submitCommand :: Handle -> Party -> Command -> IO (Either String ())
submitCommand Handle{lid} party com = do
    cid <- randomCid
    run 5 $ Ledger.submit (Commands {lid,wid,aid=myAid,cid,party,leTime,mrTime,coms=[com]})
    where
        wid = Nothing
        leTime = Timestamp 0 0
        mrTime = Timestamp 5 0
        myAid = ApplicationId "davl-console"

randomCid :: IO CommandId
randomCid = do fmap (CommandId . Text.pack . UUID.toString) randomIO
