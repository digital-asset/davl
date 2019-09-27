-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- Davl.Contracts, both sent-to, and coming-from the external ledger.
module Davl.Contracts (
    DavlContractId,
    DavlCommand(..),
    DavlTemplate(..), DavlContract(..),
    makeLedgerCommand, extractTransaction,
    ) where

import qualified Data.Text.Lazy as Text(unpack)

import DA.Ledger (
    PackageId,Command,ModuleName(..),EntityName(..),TemplateId(..),Identifier(..),
    Command(..), Event(..), Transaction(..), Choice(..),
    ContractId(..),
    Value(..), Record(..),
    )
import Davl.Domain (Gift,Holiday)
import Davl.Logging (Logger)
import DA.Ledger.IsLedgerValue (toRecord,fromRecord)

data DavlCommand
    = GiveGift Gift
    | ClaimGift DavlContractId

makeLedgerCommand :: PackageId -> DavlCommand -> Command
makeLedgerCommand pid = \case

    GiveGift x -> do
        let mod = ModuleName "Davl"
        let ent = EntityName "Gift"
        let tid = TemplateId (Identifier pid mod ent)
        let args = toRecord x
        CreateCommand {tid,args}

    ClaimGift DavlContractId{cid} -> do
        let mod = ModuleName "Davl"
        let ent = EntityName "Gift"
        let tid = TemplateId (Identifier pid mod ent)
        let choice = Choice "Gift_Claim"
        let arg = VRecord (Record Nothing [])
        ExerciseCommand {tid,cid,choice,arg}

data DavlTemplate
    = Gift Gift
    | Holiday Holiday

data DavlContract = DavlContract { id :: DavlContractId, info :: DavlTemplate }

newtype DavlContractId = DavlContractId { cid :: ContractId }

instance Show DavlContractId where
    show DavlContractId{cid} = "[" <> Text.unpack (unContractId cid) <> "]"

extractEvents :: [Event] -> Maybe DavlContract
extractEvents = \case

    [CreatedEvent{cid,tid=TemplateId Identifier{ent=EntityName"Gift"}, createArgs}] -> do
        x <- fromRecord createArgs
        return $ DavlContract { id = DavlContractId cid , info = Gift x }

    [ArchivedEvent{},
     CreatedEvent{cid,tid=TemplateId Identifier{ent=EntityName"Holiday"}, createArgs}] -> do
        x <- fromRecord createArgs
        return $ DavlContract { id = DavlContractId cid , info = Holiday x }

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
