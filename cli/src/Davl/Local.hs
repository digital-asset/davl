-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- The local state maintained non-persistently by the chat-console
module Davl.Local (
    State, initState, history,
    UserCommand(..), externalizeCommand,
    applyTransQuiet, applyTrans, Event,
    ) where

import Davl.Contracts (DavlContract)
import Davl.Domain (Party,Holiday(..),Gift(..))
import qualified Davl.Contracts as C

-- user commands, to be interpreted w.r.t the local state

data UserCommand
    = GiveTo Party
    deriving Show

-- local state, accumulates external transitions

data State = State { events :: [Event] }
    deriving (Show)

history :: State -> [Event]
history = reverse . events

initState :: State
initState = State {events = []}

-- externalize a user-centric command into a Davl Contract (creation)

externalizeCommand :: Party -> State -> UserCommand -> Either String DavlContract
externalizeCommand whoami State{} = \case
    GiveTo employee ->
        return $ C.Gift $ Gift { allocation = Holiday { boss = whoami, employee } }

-- accumulate an external Davl Contract (transaction) into the local state

applyTransQuiet :: Party -> State -> DavlContract -> State
applyTransQuiet whoami s cc = s' where (s',_) = applyTrans whoami s cc

applyTrans :: Party -> State -> DavlContract -> (State,[Event])
applyTrans whoami s@State{events} = \case
    C.Gift Gift{allocation=Holiday{boss,employee}} ->
        (s { events = event : events }, [event])
        where
            event =
                if boss == whoami
                then AGiftSent{to=employee}
                else AGiftIn{from=boss}

data Event
    = AGiftIn { from :: Party }
    | AGiftSent { to :: Party }

instance Show Event where
    show = \case
        AGiftIn{from} -> "Gift <-- " ++ show from
        AGiftSent{to} -> "Gift --> " ++ show to
