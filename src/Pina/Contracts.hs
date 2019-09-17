-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- Pina Contracts, both sent-to, and coming-from the external ledger.
module Pina.Contracts (
    PinaContract(..),
    makeLedgerCommand,extractTransaction,
    ) where

import DA.Ledger (
    PackageId,Command,ModuleName(..),EntityName(..),TemplateId(..),Identifier(..),
    Command(..), Event(..), Transaction(..)
    )

import Pina.Domain (Holiday,Gift)
import Pina.Logging (Logger)
import DA.Ledger.IsLedgerValue (toRecord,fromRecord)

data PinaContract
    = Holiday Holiday
    | Gift Gift

makeLedgerCommand :: PackageId -> PinaContract -> Command
makeLedgerCommand pid = \case
    Holiday x -> do
        let mod = ModuleName "Pina"
        let ent = EntityName "HolidayAllocation"
        let tid = TemplateId (Identifier pid mod ent)
        let args = toRecord x
        CreateCommand {tid,args}
    Gift x -> do
        let mod = ModuleName "Pina"
        let ent = EntityName "HolidayGift"
        let tid = TemplateId (Identifier pid mod ent)
        let args = toRecord x
        CreateCommand {tid,args}

extractEvents :: [Event] -> Maybe PinaContract
extractEvents = \case
    [CreatedEvent{tid=TemplateId Identifier{ent=EntityName"HolidayAllocation"}, createArgs}] -> do
        x <- fromRecord createArgs
        return $ Holiday x
    [CreatedEvent{tid=TemplateId Identifier{ent=EntityName"HolidayGift"}, createArgs}] -> do
        x <- fromRecord createArgs
        return $ Gift x
    _ ->
        Nothing

extractTransaction :: Logger -> Transaction -> IO (Maybe PinaContract)
extractTransaction log Transaction{events} = do
    case extractEvents events of
        x@(Just _) -> return x
        Nothing -> do
            log "Surprising ledger transaction events: "
            mapM_ (\e -> log $ "- " <> show e) events
            return Nothing
