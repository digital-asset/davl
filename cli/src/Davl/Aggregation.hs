-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Davl.Aggregation (
    SummaryAsBoss, summaryAsBoss,
    SummaryAsEmployee, summaryAsEmployee,
    ) where

import Data.List as List
import Data.List.Key as LK(sort)
import Data.Map.Strict as Map
import Data.Map.Merge.Strict as Map

import Davl.Domain
import Davl.ContractStore(State,activeContracts)

data Counts = Counts
    { unclaimed_gifts :: Int
    , unspent_holidays :: Int
    } deriving Show

data SummaryAsBoss = SummaryAsBoss { counts :: Map Party Counts }
data SummaryAsEmployee = SummaryAsEmployee { counts :: Map Party Counts }

instance Show SummaryAsBoss where
    show SummaryAsBoss{counts} =
        unlines ("As Boss:" : tab 2 (List.map f (LK.sort fst (Map.toList counts))))
        where f (party,count) = unwords [show party,":",show count]

instance Show SummaryAsEmployee where
    show SummaryAsEmployee{counts} =
        unlines ("As Employee:" : tab 2 (List.map f (LK.sort fst (Map.toList counts))))
        where f (party,count) = unwords [show party,":",show count]

tab :: Int -> [String] -> [String]
tab n xs = List.map ((spaces <>)) xs where spaces = replicate n ' '

summaryAsBoss :: Party -> State -> SummaryAsBoss -- grouped by Employee
summaryAsBoss whoami state = do
    let gifts = giftsAsBoss whoami state
    let holidays = holidaysAsBoss whoami state
    let mgs = flip groupByKey gifts $ \Gift{allocation=Holiday{employee}} -> employee
    let mhs = flip groupByKey holidays $ \Holiday{employee} -> employee
    let counts = flip Map.map (zipMapOfLists mgs mhs) $ \(gs,hs) ->
            Counts { unclaimed_gifts = length $ gs
                   , unspent_holidays = length $ hs
                   }
    SummaryAsBoss {counts}

summaryAsEmployee :: Party -> State -> SummaryAsEmployee -- grouped by Boss
summaryAsEmployee whoami state = do
    let gifts = giftsAsEmployee whoami state
    let holidays = holidaysAsEmployee whoami state
    let mgs = flip groupByKey gifts $ \Gift{allocation=Holiday{boss}} -> boss
    let mhs = flip groupByKey holidays $ \Holiday{boss} -> boss
    let counts = flip Map.map (zipMapOfLists mgs mhs) $ \(gs,hs) ->
            Counts { unclaimed_gifts = length $ gs
                   , unspent_holidays = length $ hs
                   }
    SummaryAsEmployee {counts}


groupByKey :: Ord k => (a -> k) -> [a] -> Map k [a]
groupByKey getKey elems =
    Map.fromList $ List.map (\xs -> (getKey (head xs), xs)) $ List.groupBy sameKey elems
    where sameKey a b = getKey a == getKey b

zipMapOfLists :: Ord k => Map k [v1] -> Map k [v2] -> Map k ([v1],[v2])
zipMapOfLists m1 m2 =
    Map.merge (mapMaybeMissing g1) (mapMaybeMissing g2) (zipWithMaybeMatched f) m1 m2
    where g1 _ v1 = Just (v1,[])
          g2 _ v2 = Just ([],v2)
          f _ v1 v2  = Just (v1,v2)


giftsAsBoss :: Party -> State -> [Gift]
giftsAsBoss whoami state = do
    g@Gift{allocation=Holiday{boss}} <- gifts state
    if (boss == whoami) then return g else []

holidaysAsBoss :: Party -> State -> [Holiday]
holidaysAsBoss whoami state = do
    g@Holiday{boss} <- holidays state
    if (boss == whoami) then return g else []


giftsAsEmployee :: Party -> State -> [Gift]
giftsAsEmployee whoami state = do
    g@Gift{allocation=Holiday{employee}} <- gifts state
    if (employee == whoami) then return g else []

holidaysAsEmployee :: Party -> State -> [Holiday]
holidaysAsEmployee whoami state = do
    g@Holiday{employee} <- holidays state
    if (employee == whoami) then return g else []


gifts :: State -> [Gift]
gifts state = do
    (_,TGift gift) <- activeContracts state
    return gift

holidays :: State -> [Holiday]
holidays state = do
    (_,THoliday holiday) <- activeContracts state
    return holiday
