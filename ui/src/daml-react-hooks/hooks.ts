// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

import { Choice, ContractId, Template } from "@daml/types";
import { CreateEvent, Query, Event } from '@daml/ledger';
import { useState, useContext, useEffect } from "react";
import { DamlLedgerState, DamlLedgerContext } from './context'

const useDamlState = (): DamlLedgerState => {
  const state = useContext(DamlLedgerContext);
  if (!state) {
    throw Error("Trying to use DamlLedgerContext before initializing.")
  }
  return state;
}

export const useParty = () => {
  const state = useDamlState();
  return state.party;
}

export type QueryResult<T extends object, K> = {
  contracts: CreateEvent<T, K>[];
  loading: boolean;
}

/// React Hook for a query against the `/contracts/search` endpoint of the JSON API.
export function useQuery<T extends object, K>(template: Template<T, K>): QueryResult<T, K>
export function useQuery<T extends object, K>(template: Template<T, K>, queryFactory: () => Query<T>, queryDeps: readonly unknown[]): QueryResult<T, K>
export function useQuery<T extends object, K>(template: Template<T, K>, queryFactory?: () => Query<T>, queryDeps?: readonly unknown[]): QueryResult<T, K> {
  const state = useDamlState();
  const [result, setResult] = useState<QueryResult<T, K>>({contracts: [], loading: false});
  useEffect(() => {
    setResult({contracts: [], loading: true});
    const query = queryFactory ? queryFactory() : undefined;
    const load = async () => {
      const contracts = await state.ledger.query(template, query);
      setResult({contracts, loading: false});
    };
    // eslint-disable-next-line @typescript-eslint/no-floating-promises
    load();
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [state.ledger, state.reloadToken, template, ...(queryDeps ?? [])]);
  return result;
}

export type FetchResult<T extends object, K> = {
  contract: CreateEvent<T, K> | null;
  loading: boolean;
}

/// React Hook for a lookup by key against the `/contracts/lookup` endpoint of the JSON API.
export function useFetchByKey<T extends object, K>(template: Template<T, K>, keyFactory: () => K, keyDeps: readonly unknown[]): FetchResult<T, K> {
  const state = useDamlState();
  const [result, setResult] = useState<FetchResult<T, K>>({contract: null, loading: false});
  useEffect(() => {
    const key = keyFactory();
    setResult({contract: null, loading: true});
    const load = async () => {
      const contract = await state.ledger.fetchByKey(template, key);
      setResult({contract, loading: false});
    };
    // eslint-disable-next-line @typescript-eslint/no-floating-promises
    load();
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [state.ledger, state.reloadToken, template, ...(keyDeps ?? [])]);
  return result;
}

/// React Hook that returns a function to exercise a choice and a boolean
/// indicator whether the exercise is currently running.
export const useExercise = <T extends object, C, R>(choice: Choice<T, C, R>): [(cid: ContractId<T>, argument: C) => Promise<R>, boolean] => {
  const state = useDamlState();
  const [loading, setLoading] = useState(false);

  const exercise = async (cid: ContractId<T>, argument: C) => {
    setLoading(true);
    const [result] = await state.ledger.exercise(choice, cid, argument);
    setLoading(false);
    return result;
  }
  return [exercise, loading];
}

/// React Hook that returns a function to exercise a choice by key and a boolean
/// indicator whether the exercise is currently running.
export const useExerciseByKey = <T extends object, C, R, K>(choice: Choice<T, C, R, K>): [(key: K, argument: C) => Promise<R>, boolean] => {
  const state = useDamlState();
  const [loading, setLoading] = useState(false);

  const exerciseByKey = async (key: K, argument: C) => {
    setLoading(true);
    const [result] = await state.ledger.exerciseByKey(choice, key, argument);
    setLoading(false);
    return result;
  }
  return [exerciseByKey, loading];
}

/// React Hook for a query against the `/contracts/searchForever` endpoint of the JSON API.
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export function useStreamQuery<T extends object, K>(template: Template<T, K>): QueryResult<T, K>
export function useStreamQuery<T extends object, K>(template: Template<T, K>, queryFactory: () => Query<T>, queryDeps: readonly unknown[]): QueryResult<T, K>
export function useStreamQuery<T extends object, K>(template: Template<T, K>, queryFactory?: () => Query<T>, queryDeps?: readonly unknown[]): QueryResult<T, K> {
  const [result, setResult] = useState<QueryResult<T, K>>({contracts: [], loading: false});
  const state = useDamlState();
  useEffect(() => {
    setResult({contracts: [], loading: true});
    const query = queryFactory ? queryFactory() : undefined;
    console.log(`mount useStreamQuery(${template.templateId}, ...)`, query);
    const onEvents = (events: Event<T, K>[]) => setResult(result => {
      const archiveEvents: Set<ContractId<T>> = new Set();
      const createEvents: CreateEvent<T, K>[] = [];
      for (const event of events) {
        if ('created' in event) {
          createEvents.push(event.created);
        } else { // i.e. 'archived' in event
          archiveEvents.add(event.archived.contractId);
        }
      }
      const contracts = result.contracts
        .concat(createEvents)
        .filter(contract => !archiveEvents.has(contract.contractId));
      return {...result, contracts};
    });
    const close = state.ledger.streamQuery(template, onEvents, query);
    setResult(result => ({...result, loading: false}));
    const cleanUp = () => {
      console.log(`unmount useStreamQuery(${template.templateId}, ...)`, query);
      close();
    }
    return cleanUp;
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [state.ledger, template, ...(queryDeps ?? [])]);
  return result;
}

/// React Hook to reload all queries currently present in the store.
export const useReload = (): () => void => {
  const state = useDamlState();
  return () => state.triggerReload();
}
