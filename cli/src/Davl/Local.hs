-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- The local state maintained non-persistently by the chat-console
module Davl.Local (
    State, initState, history,
    UserCommand(..), externalizeCommand,
    applyTransQuiet, applyTrans, Event,
    ) where

import Data.Maybe

import Davl.Contracts (DavlContractId, DavlContract(..), DavlCommand(..))
import Davl.Domain (Party,Holiday(..),Gift(..))
import qualified Davl.Contracts as C

-- user commands, to be interpreted w.r.t the local state

data UserCommand
    = GiveTo Party
    | ClaimFrom Party
    deriving Show

-- local state, accumulates external transitions

data State = State { events :: [Event] }
    deriving (Show)

history :: State -> [Event]
history = reverse . events

initState :: State
initState = State {events = []}

-- externalize a user-centric command into a Davl Contract (creation)

externalizeCommand :: Party -> State -> UserCommand -> Either String DavlCommand
externalizeCommand whoami state = \case
    GiveTo employee ->
        return $ C.GiveGift $ Gift { allocation = Holiday { boss = whoami, employee } }
    ClaimFrom boss -> do
        case findGiftToMeFrom state boss of
            Nothing -> Left $ "no gift found from: " <> show boss
            Just id -> return $ C.ClaimGift id

findGiftToMeFrom :: State ->  Party -> Maybe DavlContractId
findGiftToMeFrom (State events) bossK = listToMaybe $ do
    events >>= \case
        AGiftIn id boss ->
            if boss/=bossK then [] else return id

        _ -> []
        --AGiftSent _ _ -> []
        --AHolidayIn _ _ -> []
        --AHolidaySent _ _ -> []

-- accumulate an external Davl Contract (transaction) into the local state

applyTransQuiet :: Party -> State -> DavlContract -> State
applyTransQuiet whoami s cc = s' where (s',_) = applyTrans whoami s cc

applyTrans :: Party -> State -> DavlContract -> (State,[Event])
applyTrans whoami s@State{events} = \case
    DavlContract { id, info = C.Gift Gift{allocation=Holiday{boss,employee}} } ->
        (s { events = event : events }, [event])
        where
            event =
                if boss == whoami
                then AGiftSent{id,to=employee}
                else AGiftIn{id,from=boss}
    DavlContract { id, info = C.Holiday Holiday {boss,employee} } -> do
        let
            event =
                if boss == whoami
                then AHolidaySent{id,to=employee}
                else AHolidayIn{id,from=boss}
        (s { events = event : events }, [event])


data Event
    = AGiftIn { id :: DavlContractId, from :: Party }
    | AGiftSent { id :: DavlContractId, to :: Party }
    | AHolidayIn { id :: DavlContractId, from :: Party }
    | AHolidaySent { id :: DavlContractId, to :: Party }

instance Show Event where
    show = \case
        AGiftIn{id,from} -> unwords ["Gift", show id, "<--", show from]
        AGiftSent{id,to} -> unwords ["Gift", show id, "-->", show to]

        AHolidayIn{id,from} -> unwords ["Holiday", show id, "<--", show from]
        AHolidaySent{id,to} -> unwords ["Holiday", show id, "-->", show to]
