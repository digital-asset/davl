-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- Chat domain types. These should be derived automatically from the Daml model.

{-# LANGUAGE DuplicateRecordFields #-}

module Pina.Domain (
    Party(..),
    Holiday(..),
    Gift(..),
    ) where

import DA.Ledger.Types(Party(..),Record(..),RecordField(..),Value(..))
import DA.Ledger.IsLedgerValue (IsLedgerValue(..))

data Holiday = Holiday { employee :: Party, boss :: Party }
    deriving Show

instance IsLedgerValue Holiday where
    toValue Holiday{employee,boss} = VList [toValue employee, toValue boss]
    fromValue = \case
        VList [v1,v2] -> do
            employee <- fromValue v1
            boss <- fromValue v2
            return Holiday{employee,boss}
        _ -> Nothing

data Gift = Gift { allocation :: Holiday }
    deriving Show

instance IsLedgerValue Gift where
    toValue Gift{allocation=Holiday{boss,employee}} = VList [
        VRecord Record{rid=Nothing,
                       fields=[
                              RecordField{label="boss",fieldValue=toValue boss},
                              RecordField{label="employee",fieldValue=toValue employee}]}
        ]
    fromValue = \case
        VList [VRecord Record{
                      fields=[
                              RecordField{fieldValue=v1},
                              RecordField{fieldValue=v2}]}] -> do
            employee <- fromValue v1
            boss <- fromValue v2
            return Gift{allocation=Holiday{boss,employee}}
        _ -> Nothing
