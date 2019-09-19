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

import Davl.Domain (Gift)
import Davl.Logging (Logger)
import DA.Ledger.IsLedgerValue (toRecord,fromRecord)

data DavlContract
    = Gift Gift

makeLedgerCommand :: PackageId -> DavlContract -> Command
makeLedgerCommand pid = \case
    Gift x -> do
        let mod = ModuleName "Davl"
        let ent = EntityName "Gift"
        let tid = TemplateId (Identifier pid mod ent)
        let args = toRecord x
        CreateCommand {tid,args}

extractEvents :: [Event] -> Maybe DavlContract
extractEvents = \case
    [CreatedEvent{tid=TemplateId Identifier{ent=EntityName"Gift"}, createArgs}] -> do
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
