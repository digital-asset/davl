-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- Davl Contracts, both sent-to, and coming-from the external ledger.
module Davl.Contracts (
    DavlContract(..),
    makeLedgerCommand,extractTransaction,
    ) where

import DA.Ledger (
    PackageId,Command,ModuleName(..),EntityName(..),TemplateId(..),Identifier(..),
    Command(..), Event(..), Transaction(..)
    )

import Davl.Domain (Holiday,Gift)
import Davl.Logging (Logger)
import DA.Ledger.IsLedgerValue (toRecord,fromRecord)

data DavlContract
    = Holiday Holiday
    | Gift Gift

makeLedgerCommand :: PackageId -> DavlContract -> Command
makeLedgerCommand pid = \case
    Holiday x -> do
        let mod = ModuleName "Davl"
        let ent = EntityName "HolidayAllocation"
        let tid = TemplateId (Identifier pid mod ent)
        let args = toRecord x
        CreateCommand {tid,args}
    Gift x -> do
        let mod = ModuleName "Davl"
        let ent = EntityName "HolidayGift"
        let tid = TemplateId (Identifier pid mod ent)
        let args = toRecord x
        CreateCommand {tid,args}

extractEvents :: [Event] -> Maybe DavlContract
extractEvents = \case
    [CreatedEvent{tid=TemplateId Identifier{ent=EntityName"HolidayAllocation"}, createArgs}] -> do
        x <- fromRecord createArgs
        return $ Holiday x
    [CreatedEvent{tid=TemplateId Identifier{ent=EntityName"HolidayGift"}, createArgs}] -> do
        x <- fromRecord createArgs
        return $ Gift x
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
