-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Davl.Domain (
    Party(..),
    Holiday(..),
    Gift(..),
    DavlContractId(..), DavlTemplate(..), DavlEvent(..),
    DavlCommand(..)
    ) where

import qualified Data.Text.Lazy as Text(unpack)

import DA.Ledger.Types (Party(..),ContractId(..))

data Holiday = Holiday { employee :: Party, boss :: Party } deriving Show

data Gift = Gift { allocation :: Holiday } deriving Show

data DavlTemplate
    = TGift Gift
    | THoliday Holiday

newtype DavlContractId = DavlContractId { cid :: ContractId } deriving (Eq)

data DavlEvent
    = Create { id :: DavlContractId, info :: DavlTemplate }
    | Archive DavlContractId

data DavlCommand
    = GiveGift Gift
    | ClaimGift DavlContractId

instance Show DavlTemplate where
    show = \case
        TGift x -> show x
        THoliday x -> show x

instance Show DavlEvent where
    show = \case
        Create {id,info} -> "CREATE: " <> show id <> " = " <> show info
        Archive id -> "ARCHIVE: " <> show id
instance Show DavlContractId where
    show DavlContractId{cid} = "[" <> Text.unpack (unContractId cid) <> "]"
