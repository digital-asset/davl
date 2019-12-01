import { Template, Query, Contract, Choice, ContractId, lookupTemplate } from "@digitalasset/daml-json-types";
import Ledger, { Event } from "../ledger/ledger";
import { useEffect, useMemo, useState, useContext } from "react";
import * as LedgerStore from './ledgerStore';
import * as TemplateStore from './templateStore';
import React from "react";

export type Store = {
  data: LedgerStore.Store;
  dispatch: React.Dispatch<Action>;
  ledger: Ledger;
}

export const DamlLedgerContext = React.createContext(null as Store | null);

const SET_QUERY_LOADING = 'SET_QUERY_LOADING';
const SET_QUERY_RESULT = 'SET_QUERY_RESULT';

type SetQueryLoadingAction<T> = {
  type: typeof SET_QUERY_LOADING;
  template: Template<T>;
  query: Query<T>;
}

type SetQueryResultAction<T> = {
  type: typeof SET_QUERY_RESULT;
  template: Template<T>;
  query: Query<T>;
  contracts: Contract<T>[];
}

export type Action = SetQueryLoadingAction<object> | SetQueryResultAction<object>;

const setQueryLoading = <T>(template: Template<T>, query: Query<T>): SetQueryLoadingAction<T> => ({
  type: SET_QUERY_LOADING,
  template,
  query,
});

const setQueryResult = <T>(template: Template<T>, query: Query<T>, contracts: Contract<T>[]): SetQueryResultAction<T> => ({
  type: SET_QUERY_RESULT,
  template,
  query,
  contracts,
});

export const reducer = (ledgerStore: LedgerStore.Store, action: Action): LedgerStore.Store => {
  switch (action.type) {
    case SET_QUERY_LOADING: {
      return LedgerStore.setQueryLoading(ledgerStore, action.template, action.query);
    }
    case SET_QUERY_RESULT: {
      return LedgerStore.setQueryResult(ledgerStore, action.template, action.query, action.contracts);
    }
  }
}

const useStore = (): Store => {
  const store = useContext(DamlLedgerContext);
  if (!store) {
    throw Error("Trying to use DamlLedgerContext before initializing.")
  }
  return store;
}

export const useParty = () => {
  const store = useStore();
  return store.ledger.party;
}

const loadQuery = async <T extends {}>(store: Store, template: Template<T>, query: Query<T>) => {
  store.dispatch(setQueryLoading(template, query));
  const contracts = await store.ledger.query(template, query);
  store.dispatch(setQueryResult(template, query, contracts));
}

const emptyQueryFactory = <T extends {}>(): Query<T> => ({} as Query<T>);

export type QueryResult<T> = {
  contracts: Contract<T>[];
  loading: boolean;
}

/// React Hook for a query against the `/contracts/search` endpoint of the JSON API.
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export const useQuery = <T>(template: Template<T>, queryFactory: () => Query<T> = emptyQueryFactory, queryDeps?: readonly any[]): QueryResult<T> => {
  const store = useStore();
  const query = useMemo(queryFactory, queryDeps);
  const contracts = LedgerStore.getQueryResult(store.data, template, query);
  useEffect(() => {
    if (contracts === undefined) {
      // eslint-disable-next-line @typescript-eslint/no-floating-promises
      loadQuery(store, template, query);
    }
  }, [store, template, query, contracts]);
  return contracts || TemplateStore.emptyQueryResult();
}

export type FetchResult<T> = {
  contract: Contract<T> | null;
  loading: boolean;
}

/// React Hook for a query against the `/contracts/seach` endpoint that yields
/// at most one contract. This can be thought of as a poor man's version of
/// `fetchByKey`.
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export const usePseudoFetchByKey = <T>(template: Template<T>, keyFactory: () => Query<T>, keyDeps?: readonly any[]): FetchResult<T> => {
  const entry = useQuery(template, keyFactory, keyDeps);
  if (entry.contracts.length > 1) {
    throw Error("usePseudoFetchByKey: query returned multiple cotracts");
  }
  return useMemo(() => ({
    loading: entry.loading,
    contract: entry.contracts[0] || null,
  }), [entry]);
}

const reloadTemplate = async <T extends {}>(store: Store, template: Template<T>) => {
  const templateStore = store.data.templateStores.get(template) as TemplateStore.Store<T> | undefined;
  if (templateStore) {
    const queries: Query<T>[] = Array.from(templateStore.queryResults.keys());
    await Promise.all(queries.map(async (query) => {
      await loadQuery(store, template, query);
    }));
  }
}

const reloadEvents = async (store: Store, events: Event[]) => {
  // TODO(MH): This is a sledge hammer approach. We completely reload every
  // single template that has been touched by the events. A future optimization
  // would be to remove the archived templates from their tables and add the
  // created templates wherever they match.
  const templates = new Set(events.map((event) =>
    lookupTemplate('created' in event ? event.created.templateId : event.archived.templateId)
  ));
  await Promise.all(Array.from(templates).map((template) => reloadTemplate(store, template)));
}

/// React Hook that returns a function to exercise a choice and a boolean
/// indicator whether the exercise is currently running.
export const useExercise = <T, C>(choice: Choice<T, C>): [(cid: ContractId<T>, argument: C) => Promise<void>, boolean] => {
  const [loading, setLoading] = useState(false);
  const store = useStore();

  const exercise = async (cid: ContractId<T>, argument: C) => {
    setLoading(true);
    const events = await store.ledger.exercise(choice, cid, argument);
    setLoading(false);
    // NOTE(MH): We want to signal the UI that the exercise is finished while
    // were still updating the affected templates "in the backgound".
    // eslint-disable-next-line @typescript-eslint/no-floating-promises
    reloadEvents(store, events);
  }
  return [exercise, loading];
}

/// React Hook that returns a function to exercise a choice and a boolean
/// indicator whether the exercise is currently running.
export const usePseudoExerciseByKey = <T, C>(choice: Choice<T, C>): [(key: Query<T>, argument: C) => Promise<void>, boolean] => {
  const [loading, setLoading] = useState(false);
  const store = useStore();

  const exercise = async (key: Query<T>, argument: C) => {
    setLoading(true);
    const events = await store.ledger.pseudoExerciseByKey(choice, key, argument);
    setLoading(false);
    // NOTE(MH): We want to signal the UI that the exercise is finished while
    // were still updating the affected templates "in the backgound".
    // eslint-disable-next-line @typescript-eslint/no-floating-promises
    reloadEvents(store, events);
  }
  return [exercise, loading];
}

/// React Hook to reload all queries currently present in the store.
export const useReload = (): () => Promise<void> => {
  const store = useStore();
  return async () => {
    const templates = Array.from(store.data.templateStores.keys());
    await Promise.all(templates.map((template) => reloadTemplate(store, template)));
  }
}
