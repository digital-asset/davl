import { Template, Choice, ContractId, lookupTemplate } from "@digitalasset/daml-json-types";
import { Event, Query, CreateEvent } from '@digitalasset/daml-ledger-fetch';
import { useEffect, useMemo, useState, useContext } from "react";
import * as LedgerStore from './ledgerStore';
import * as TemplateStore from './templateStore';
import { setQueryLoading, setQueryResult, setFetchByKeyLoading, setFetchByKeyResult } from "./reducer";
import { DamlLedgerState, DamlLedgerContext } from './context';

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

const loadQuery = async <T extends object>(state: DamlLedgerState, template: Template<T>, query: Query<T>) => {
  state.dispatch(setQueryLoading(template, query));
  const contracts = await state.ledger.query(template, query);
  state.dispatch(setQueryResult(template, query, contracts));
}

const emptyQueryFactory = <T extends object>(): Query<T> => ({} as Query<T>);

export type QueryResult<T extends object, K> = {
  contracts: CreateEvent<T, K>[];
  loading: boolean;
}

/// React Hook for a query against the `/contracts/search` endpoint of the JSON API.
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export const useQuery = <T extends object, K>(template: Template<T, K>, queryFactory: () => Query<T> = emptyQueryFactory, queryDeps?: readonly any[]): QueryResult<T, K> => {
  const state = useDamlState();
  const query = useMemo(queryFactory, queryDeps);
  const contracts = LedgerStore.getQueryResult(state.store, template, query);
  useEffect(() => {
    if (contracts === undefined) {
      // eslint-disable-next-line @typescript-eslint/no-floating-promises
      loadQuery(state, template, query);
    }
  }, [state, template, query, contracts]);
  return contracts ?? TemplateStore.emptyQueryResult();
}

const loadFetchByKey = async <T extends object, K>(state: DamlLedgerState, template: Template<T, K>, key: K) => {
  state.dispatch(setFetchByKeyLoading(template, key));
  let contract;
  if (key === undefined) {
    console.error(`Calling useFetchByKey on template without a contract key: ${template}`);
    contract = null;
  } else {
    contract = await state.ledger.lookupByKey(template, key as K extends undefined ? never : K);
  }
  state.dispatch(setFetchByKeyResult(template, key, contract));
}

export type FetchResult<T extends object, K> = {
  contract: CreateEvent<T, K> | null;
  loading: boolean;
}

/// React Hook for a lookup by key against the `/contracts/lookup` endpoint of the JSON API.
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export const useFetchByKey = <T extends object, K>(template: Template<T, K>, keyFactory: () => K, keyDeps?: readonly any[]): FetchResult<T, K> => {
  const state = useDamlState();
  const key = useMemo(keyFactory, keyDeps);
  const contract = LedgerStore.getFetchByKeyResult(state.store, template, key);
  useEffect(() => {
    if (contract === undefined) {
      // eslint-disable-next-line @typescript-eslint/no-floating-promises
      loadFetchByKey(state, template, key);
    }
  }, [state, template, key, contract]);
  return contract ?? TemplateStore.emptyFetchResult();
}

const reloadTemplate = async <T extends object, K>(state: DamlLedgerState, template: Template<T, K>) => {
  const templateStore = state.store.templateStores.get(template) as TemplateStore.Store<T, K> | undefined;
  if (templateStore) {
    const queries: Query<T>[] = Array.from(templateStore.queryResults.keys());
    const keys: K[] = Array.from(templateStore.fetchByKeyResults.keys());
    await Promise.all([
      Promise.all(queries.map(async (query) => await loadQuery(state, template, query))),
      Promise.all(keys.map(async (key) => await loadFetchByKey(state, template, key))),
    ]);
  }
}

const reloadEvents = async (state: DamlLedgerState, events: Event<object>[]) => {
  // TODO(MH): This is a sledge hammer approach. We completely reload every
  // single template that has been touched by the events. A future optimization
  // would be to remove the archived templates from their tables and add the
  // created templates wherever they match.
  const templates = new Set(events.map((event) =>
    lookupTemplate('created' in event ? event.created.templateId : event.archived.templateId)
  ));
  await Promise.all(Array.from(templates).map((template) => reloadTemplate(state, template)));
}

/// React Hook that returns a function to exercise a choice and a boolean
/// indicator whether the exercise is currently running.
export const useExercise = <T extends object, C, R>(choice: Choice<T, C, R>): [(cid: ContractId<T>, argument: C) => Promise<R>, boolean] => {
  const [loading, setLoading] = useState(false);
  const state = useDamlState();

  const exercise = async (cid: ContractId<T>, argument: C) => {
    setLoading(true);
    const [result, events] = await state.ledger.exercise(choice, cid, argument);
    setLoading(false);
    // NOTE(MH): We want to signal the UI that the exercise is finished while
    // were still updating the affected templates "in the backgound".
    // eslint-disable-next-line @typescript-eslint/no-floating-promises
    reloadEvents(state, events);
    return result;
  }
  return [exercise, loading];
}

/// React Hook that returns a function to exercise a choice and a boolean
/// indicator whether the exercise is currently running.
export const usePseudoExerciseByKey = <T extends object, C, R>(choice: Choice<T, C, R>): [(key: Query<T>, argument: C) => Promise<R>, boolean] => {
  const [loading, setLoading] = useState(false);
  const state = useDamlState();

  const exercise = async (key: Query<T>, argument: C) => {
    setLoading(true);
    const [result, events] = await state.ledger.pseudoExerciseByKey(choice, key, argument);
    setLoading(false);
    // NOTE(MH): We want to signal the UI that the exercise is finished while
    // were still updating the affected templates "in the backgound".
    // eslint-disable-next-line @typescript-eslint/no-floating-promises
    reloadEvents(state, events);
    return result;
  }
  return [exercise, loading];
}

/// React Hook to reload all queries currently present in the store.
export const useReload = (): () => Promise<void> => {
  const state = useDamlState();
  return async () => {
    const templates = Array.from(state.store.templateStores.keys());
    await Promise.all(templates.map((template) => reloadTemplate(state, template)));
  }
}
