-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -Wno-orphans #-}

module Davl.LedgerTranslation(
    makeLedgerCommand,
    extractTransaction,
    ) where

import DA.Ledger.IsLedgerValue (IsLedgerValue(..))
import qualified DA.Ledger.Types as L

import Davl.Logging (Logger)
import Davl.Domain

instance IsLedgerValue Holiday where
    toValue Holiday{employee,boss} = L.VList [toValue employee, toValue boss]
    fromValue = \case
        L.VList [v1,v2] -> do
            employee <- fromValue v1
            boss <- fromValue v2
            return Holiday{employee,boss}
        _ -> Nothing

instance IsLedgerValue Gift where
    toValue Gift{allocation=Holiday{boss,employee}} = L.VList [
        L.VRecord L.Record{rid=Nothing,
                       fields=[
                              L.RecordField{label="boss",fieldValue=toValue boss},
                              L.RecordField{label="employee",fieldValue=toValue employee}]}
        ]
    fromValue = \case
        L.VList [L.VRecord L.Record{
                      fields=[
                              L.RecordField{fieldValue=v1},
                              L.RecordField{fieldValue=v2}]}] -> do
            employee <- fromValue v1
            boss <- fromValue v2
            return Gift{allocation=Holiday{boss,employee}}
        _ -> Nothing

makeLedgerCommand :: L.PackageId -> DavlCommand -> L.Command
makeLedgerCommand pid = \case

    GiveGift x -> do
        let mod = L.ModuleName "Davl"
        let ent = L.EntityName "Gift"
        let tid = L.TemplateId (L.Identifier pid mod ent)
        let args = toRecord x
        L.CreateCommand {tid,args}

    ClaimGift DavlContractId{cid} -> do
        let mod = L.ModuleName "Davl"
        let ent = L.EntityName "Gift"
        let tid = L.TemplateId (L.Identifier pid mod ent)
        let choice = L.Choice "Gift_Claim"
        let arg = L.VRecord (L.Record Nothing [])
        L.ExerciseCommand {tid,cid,choice,arg}

extractEvents :: [L.Event] -> Maybe [DavlEvent]
extractEvents = \case

    [L.CreatedEvent{cid,tid=L.TemplateId L.Identifier{ent=L.EntityName"Gift"}, createArgs}] -> do
        x <- fromRecord createArgs
        return $ [Create { id = DavlContractId cid , info = TGift x }]

    [L.ArchivedEvent{cid=cid1},
     L.CreatedEvent{cid=cid2,tid=L.TemplateId L.Identifier{ent=L.EntityName"Holiday"}, createArgs}] -> do
        x <- fromRecord createArgs
        return $
            [ Archive $ DavlContractId cid1
            , Create { id = DavlContractId cid2 , info = THoliday x }
            ]

    _ ->
        Nothing

extractTransaction :: Logger -> L.Transaction -> IO [DavlEvent]
extractTransaction log L.Transaction{events} = do
    case extractEvents events of
        Just xs -> return xs
        Nothing -> do
            log "Surprising ledger transaction events: "
            mapM_ (\e -> log $ "- " <> show e) events
            return []
