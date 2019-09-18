-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- The local state maintained non-persistently by the chat-console
module Pina.Local (
    State, initState, history,
    UserCommand(..), externalizeCommand,
    applyTransQuiet, applyTrans, Announce,
    ) where

import Pina.Contracts (PinaContract)
import Pina.Domain (Party,Holiday(..),Gift(..))
--import Data.List ((\\))
--import Data.Text.Lazy (Text)
--import Data.Text.Lazy as Text (unpack)
import qualified Pina.Contracts as C

-- user commands, to be interpreted w.r.t the local state

data UserCommand
    = Give
    deriving Show

-- local state, accumulates external transitions

data State = State
    {
    ans :: [Announce]
    }
    deriving (Show)

history :: State -> [Announce]
history = reverse . ans

data MKind = MSay | MShout deriving (Show)

initState :: State
initState = State
    {
     ans = []
    }

-- externalize a user-centric command into a Pina Contract (creation)

externalizeCommand :: Party -> Party -> State -> UserCommand -> Either String PinaContract
externalizeCommand whoami employee State{} = \case
    Give ->
        return $ C.Gift $ Gift { allocation = Holiday { boss = whoami, employee } }

-- accumulate an external Pina Contract (transaction) into the local state

applyTransQuiet :: Party -> State -> PinaContract -> State
applyTransQuiet whoami s cc = s' where (s',_,_) = applyTrans whoami s cc

applyTrans :: Party -> State -> PinaContract -> (State,[Announce],[PinaContract])
applyTrans whoami s@State{ans} = \case
    C.Gift Gift{allocation=Holiday{boss,employee}} ->
        (s { ans = an : ans }, {-if bo == whoami then [] else-} [an], [])
        where an =
                  if boss == whoami
                  then AGiftSent{to=employee}
                  else AGiftIn{from=boss}

    C.Holiday Holiday{boss,employee} ->
        (s { ans = an : ans }, {-if bo == whoami then [] else-} [an], [])
        where an =
                  if boss == whoami
                  then AHolidaySent{to=employee}
                  else AHolidayIn{from=boss}

data Announce
    = AGiftIn { from :: Party }
    | AGiftSent { to :: Party }
    | AHolidayIn { from :: Party }
    | AHolidaySent { to :: Party }


instance Show Announce where
    show = \case
        AGiftIn{from} -> "(GiftIn" ++ show from ++ ") "
        AGiftSent{to} -> "-> (GiftSent" ++ show to ++") "
        AHolidayIn{from} -> "(HolIn" ++ show from ++ ") "
        AHolidaySent{to} -> "-> (HolSent" ++ show to ++") "
