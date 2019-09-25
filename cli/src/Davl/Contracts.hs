-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- Davl Contracts, both sent-to, and coming-from the external ledger.
module Davl.Contracts (
    DavlContractId,
    DavlTemplate(..), DavlContract(..),
    makeLedgerCommand,extractTransaction,
    ) where

import qualified Data.Text.Lazy as Text(unpack)

import DA.Ledger (
    PackageId,Command,ModuleName(..),EntityName(..),TemplateId(..),Identifier(..),
    Command(..), Event(..), Transaction(..),
    ContractId(..),
    )
import Davl.Domain (Gift)
import Davl.Logging (Logger)
import DA.Ledger.IsLedgerValue (toRecord,fromRecord)

data DavlTemplate
    = Gift Gift

makeLedgerCommand :: PackageId -> DavlTemplate -> Command
makeLedgerCommand pid = \case
    Gift x -> do
        let mod = ModuleName "Davl"
        let ent = EntityName "Gift"
        let tid = TemplateId (Identifier pid mod ent)
        let args = toRecord x
        CreateCommand {tid,args}

data DavlContract = DavlContract { id :: DavlContractId, info :: DavlTemplate }

newtype DavlContractId = DavlContractId { cid :: String }

instance Show DavlContractId where
    show DavlContractId{cid} = "[" <> cid <> "]"


extractEvents :: [Event] -> Maybe DavlContract
extractEvents = \case
    [CreatedEvent{cid,tid=TemplateId Identifier{ent=EntityName"Gift"}, createArgs}] -> do
        x <- fromRecord createArgs
        return $ DavlContract { id = DavlContractId $ Text.unpack $ unContractId $ cid , info = Gift x }
    _ ->
        Nothing

extractTransaction :: Logger -> Transaction -> IO (Maybe DavlContract)
extractTransaction log Transaction{events} = do
    case extractEvents events of
        x@(Just _) -> return x
        Nothing -> do
            log "Surprising ledger transaction events: "
            mapM_ (\e -> log $ "- " <> show e) events
            return Nothing
