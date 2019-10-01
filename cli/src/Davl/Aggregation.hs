-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Davl.Aggregation (
    SummaryAsBoss, summaryAsBoss,
    SummaryAsEmployee, summaryAsEmployee,
    ) where

import Data.List as List
import Data.List.Key as LK(sort)
import Data.Map.Strict as Map

import Davl.Domain
import Davl.ContractStore(State,activeContracts)

data Counts = Counts
    { gifts :: Int -- unclaimed gifts
    , holidays :: Int -- unspent holiday (TODO: don't count allocations with pending requests)
    , requests :: Int -- requests pending an answer: deny/approve
    } deriving Show

counts0 :: Counts
counts0 = Counts
    { gifts = 0
    , holidays = 0
    , requests = 0
    }

-- TODO: rename counts -> grouped
data SummaryAsBoss = SummaryAsBoss { counts :: Map Party Counts }
data SummaryAsEmployee = SummaryAsEmployee { counts :: Map Party Counts }

instance Show SummaryAsBoss where
    show SummaryAsBoss{counts} =
        unlines ("As Boss (grouped by Employee):" :
                 tab 2 (List.map f (LK.sort fst (Map.toList counts))))
        where f (party,count) = unwords [show party,":",show count]

instance Show SummaryAsEmployee where
    show SummaryAsEmployee{counts} =
        unlines ("As Employee (grouped by Boss)" :
                 tab 2 (List.map f (LK.sort fst (Map.toList counts))))
        where f (party,count) = unwords [show party,":",show count]

tab :: Int -> [String] -> [String]
tab n xs = List.map ((spaces <>)) xs where spaces = replicate n ' '


summaryAsEmployee :: Party -> State -> SummaryAsEmployee
summaryAsEmployee whoami state =
    SummaryAsEmployee (genericSummary iAmEmployee tickBoss whoami state)

summaryAsBoss :: Party -> State -> SummaryAsBoss
summaryAsBoss whoami state =
    SummaryAsBoss (genericSummary iAmBoss tickEmployee whoami state)

type FILTER = (DavlTemplate -> Bool)
type TICK = (Map Party Counts -> Map Party Counts)
type INC = (Counts -> Counts)

iAmBoss,iAmEmployee :: Party -> FILTER
tickBoss,tickEmployee :: DavlTemplate -> TICK
incGifts,incHolidays,incRequests :: INC

genericSummary
    :: (Party -> FILTER)
    -> (DavlTemplate -> TICK)
    -> Party -> State -> Map Party Counts
genericSummary iAmRole tickRole whoami state = counts
    where
        counts = List.foldr tickRole Map.empty $ List.filter (iAmRole whoami) templates
        templates = List.map snd $ activeContracts state

-- iAm*
iAmBoss whoami = (whoami ==) . \case
    TGift Gift{allocation=Holiday{boss}} -> boss
    THoliday Holiday{boss} -> boss
    TRequest Request{boss} -> boss
    TDenial Denial{boss} -> boss
    TVacation Vacation{boss} -> boss

iAmEmployee whoami = (whoami ==) . \case
    TGift Gift{allocation=Holiday{employee}} -> employee
    THoliday Holiday{employee} -> employee
    TRequest Request{employee} -> employee
    TDenial Denial{employee} -> employee
    TVacation Vacation{employee} -> employee

-- tick*
tickEmployee = \case
    TGift Gift{allocation=Holiday{employee}} -> finding employee incGifts
    THoliday Holiday{employee} -> finding employee incHolidays
    TRequest Request{employee} -> finding employee incRequests
    TDenial Denial{employee=_} -> Prelude.id -- TODO
    TVacation Vacation{employee=_} -> Prelude.id -- TODO

tickBoss = \case
    TGift Gift{allocation=Holiday{boss}} -> finding boss incGifts
    THoliday Holiday{boss} -> finding boss incHolidays
    TRequest Request{boss} -> finding boss incRequests
    TDenial Denial{boss=_} -> Prelude.id -- TODO
    TVacation Vacation{boss=_} -> Prelude.id -- TODO

finding :: Party -> INC -> TICK
finding key f m = do
    let counts = case m !? key of Nothing -> counts0; Just counts -> counts
    Map.insert key (f counts) m

-- inc*
incGifts c@Counts{gifts} = c { gifts = gifts + 1 }
incHolidays c@Counts{holidays} = c { holidays = holidays + 1 }
incRequests c@Counts{requests} = c { requests = requests + 1 }
