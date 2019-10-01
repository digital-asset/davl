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
    , pending_requests :: Int
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
    let requests = requestsAsBoss whoami state
    let mgs = flip groupByKey gifts $ \Gift{allocation=Holiday{employee}} -> employee
    let mhs = flip groupByKey holidays $ \Holiday{employee} -> employee
    let mrs = flip groupByKey requests $ \Request{employee} -> employee
    let counts = flip Map.map (zipMap3 mgs mhs mrs) $ \(gs,hs,rs) ->
            Counts { unclaimed_gifts = length gs
                   , unspent_holidays = length hs
                   , pending_requests = length rs
                   }
    SummaryAsBoss {counts}

summaryAsEmployee :: Party -> State -> SummaryAsEmployee -- grouped by Boss
summaryAsEmployee whoami state = do
    let gifts = giftsAsEmployee whoami state
    let holidays = holidaysAsEmployee whoami state
    let requests = requestsAsEmployee whoami state
    let mgs = flip groupByKey gifts $ \Gift{allocation=Holiday{boss}} -> boss
    let mhs = flip groupByKey holidays $ \Holiday{boss} -> boss
    let mrs = flip groupByKey requests $ \Request{boss} -> boss
    let counts = flip Map.map (zipMap3 mgs mhs mrs) $ \(gs,hs,rs) ->
            Counts { unclaimed_gifts = length gs
                   , unspent_holidays = length hs
                   , pending_requests = length rs
                   }
    SummaryAsEmployee {counts}


groupByKey :: Ord k => (a -> k) -> [a] -> Map k [a]
groupByKey getKey elems =
    Map.fromList $ List.map (\xs -> (getKey (head xs), xs)) $ List.groupBy sameKey elems
    where sameKey a b = getKey a == getKey b

zipMap2 :: Ord k => Map k [v1] -> Map k [v2] -> Map k ([v1],[v2])
zipMap2 m1 m2 =
    Map.merge (mapMaybeMissing g1) (mapMaybeMissing g2) (zipWithMaybeMatched f) m1 m2
    where g1 _ v1 = Just (v1,[])
          g2 _ v2 = Just ([],v2)
          f _ v1 v2  = Just (v1,v2)

zipMap3 :: Ord k => Map k [v1] -> Map k [v2] -> Map k [v3] -> Map k ([v1],[v2],[v3])
zipMap3 m1 m2 m3 =
    Map.merge (mapMaybeMissing g12) (mapMaybeMissing g3) (zipWithMaybeMatched f) m12 m3
    where m12 = zipMap2 m1 m2
          g12 _ (v1,v2) = Just (v1,v2,[])
          g3 _ v3 = Just ([],[],v3)
          f _ (v1,v2) v3  = Just (v1,v2,v3)

-- TODO: move -> common/shared state queries

giftsAsBoss :: Party -> State -> [Gift]
giftsAsBoss whoami state = do
    g@Gift{allocation=Holiday{boss}} <- gifts state
    if (boss == whoami) then return g else []

holidaysAsBoss :: Party -> State -> [Holiday]
holidaysAsBoss whoami state = do
    g@Holiday{boss} <- holidays state
    if (boss == whoami) then return g else []

requestsAsBoss :: Party -> State -> [Request]
requestsAsBoss whoami state = do
    x@Request{boss} <- requests state
    if (boss == whoami) then return x else []


giftsAsEmployee :: Party -> State -> [Gift]
giftsAsEmployee whoami state = do
    g@Gift{allocation=Holiday{employee}} <- gifts state
    if (employee == whoami) then return g else []

holidaysAsEmployee :: Party -> State -> [Holiday]
holidaysAsEmployee whoami state = do
    g@Holiday{employee} <- holidays state
    if (employee == whoami) then return g else []

requestsAsEmployee :: Party -> State -> [Request]
requestsAsEmployee whoami state = do
    x@Request{employee} <- requests state
    if (employee == whoami) then return x else []


gifts :: State -> [Gift]
gifts state = do
    (_,TGift gift) <- activeContracts state
    return gift

holidays :: State -> [Holiday]
holidays state = do
    (_,THoliday holiday) <- activeContracts state
    return holiday

requests :: State -> [Request]
requests state = do
    (_,TRequest request) <- activeContracts state
    return request
