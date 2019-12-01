/* eslint-disable @typescript-eslint/no-namespace */
import { Template, Query, Contract, Choice, ContractId, lookupTemplate } from "@digitalasset/daml-json-types";
import Ledger, { Event } from "../ledger/ledger";
import * as QueryStore from './query';
import { useEffect, useMemo, useState, useContext } from "react";
import * as LedgerStore from './ledgerStore';
import * as TemplateStore from './templateStore';
import React from "react";

export type Store = LedgerStore.Store;

export type StoreHandle = [Store, React.Dispatch<Action>];

export const DamlLedgerContext = React.createContext(null as StoreHandle | null);

const useStore = (): StoreHandle => {
  const handle = useContext(DamlLedgerContext);
  if (!handle) {
    throw Error("Trying to use DamlLedgerContext before initializing.")
  }
  return handle;
}

const SET_QUERY_LOADING = 'daml/SET_QUERY_LOADING';
const SET_QUERY_RESULT = 'daml/SET_QUERY_RESULT';

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

const loadQuery = async <T extends {}>(store: Store, dispatch: React.Dispatch<Action>, template: Template<T>, query: Query<T>) => {
  dispatch(setQueryLoading(template, query));
  const ledger = new Ledger(store.credentials);
  const contracts = await ledger.query(template, query);
  dispatch(setQueryResult(template, query, contracts));
}

const reloadTemplate = async <T extends {}>(store: Store, dispatch: React.Dispatch<Action>, template: Template<T>) => {
  const templateStore = store.templateStores.get(template) as TemplateStore.Store<T> | undefined;
  if (templateStore) {
    const queries: Query<T>[] = Array.from(templateStore.queryResults.keys());
    await Promise.all(queries.map(async (query) => {
      await loadQuery(store, dispatch, template, query);
    }));
  }
}

export const reload = async (store: Store, dispatch: React.Dispatch<Action>) => {
  await Promise.all(store.templateStores.toArray().map(([template]) =>
    reloadTemplate(store, dispatch, template)
  ));
}

const reloadForEvents = async (store: Store, dispatch: React.Dispatch<Action>, events: Event[]) => {
  // TODO(MH): This is a sledge hammer approach. We completely reload every
  // single template that has been touched by the events. A future optimization
  // would be to remove the archived templates from their tables and add the
  // created templates wherever they match.
  const templates = new Set(events.map((event) =>
    lookupTemplate('created' in event ? event.created.templateId : event.archived.templateId)
  ));
  await Promise.all(Array.from(templates).map((template) => reloadTemplate(store, dispatch, template)));
}

const runExercise = async <T, C>(store: Store, dispatch: React.Dispatch<Action>, choice: Choice<T, C>, cid: ContractId<T>, argument: C) => {
  const ledger = new Ledger(store.credentials);
  const events = await ledger.exercise(choice, cid, argument);
  // NOTE(MH): We want to signal the UI that the exercise is finished while
  // were still updating the affected templates "in the backgound".
  // eslint-disable-next-line @typescript-eslint/no-floating-promises
  reloadForEvents(store, dispatch, events);
}

const runPseudoExerciseByKey = async <T, C>(store: Store, dispatch: React.Dispatch<Action>, choice: Choice<T, C>, key: Query<T>, argument: C) => {
  const ledger = new Ledger(store.credentials);
  const events = await ledger.pseudoExerciseByKey(choice, key, argument);
  // NOTE(MH): We want to signal the UI that the exercise is finished while
  // were still updating the affected templates "in the backgound".
  // eslint-disable-next-line @typescript-eslint/no-floating-promises
  reloadForEvents(store, dispatch, events);
}

export const reducer = (state: Store, action: Action): Store => {
  switch (action.type) {
    case SET_QUERY_LOADING: {
      return LedgerStore.setQueryLoading(state, action.template, action.query);
    }
    case SET_QUERY_RESULT: {
      return LedgerStore.setQueryResult(state, action.template, action.query, action.contracts);
    }
  }
  return state;
}

const emptyQueryFactory = <T extends {}>(): Query<T> => ({} as Query<T>);

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export const useQuery = <T>(template: Template<T>, queryFactory: () => Query<T> = emptyQueryFactory, queryDeps?: readonly any[]): QueryStore.Entry<T> => {
  const [store, dispatch] = useStore();
  const query = useMemo(queryFactory, queryDeps);
  const contracts = LedgerStore.getQueryResult(store, template, query);
  useEffect(() => {
    if (contracts === undefined) {
      // eslint-disable-next-line @typescript-eslint/no-floating-promises
      loadQuery(store, dispatch, template, query);
    }
  }, [store, dispatch, template, query, contracts]);
  return contracts || TemplateStore.emptyQueryResult();
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export const usePseudoFetchByKey = <T>(template: Template<T>, keyFactory: () => Query<T>, keyDeps?: readonly any[]): QueryStore.KeyEntry<T> => {
  const entry = useQuery(template, keyFactory, keyDeps);
  if (entry.contracts.length > 1) {
    throw Error("usePseudoFetchByKey: query returned multiple cotracts");
  }
  return useMemo(() => ({
    loading: entry.loading,
    contract: entry.contracts[0] || null,
  }), [entry]);
}

export const useExercise = <T, C>(choice: Choice<T, C>): [(cid: ContractId<T>, argument: C) => Promise<void>, boolean] => {
  const [loading, setLoading] = useState(false);
  const [store, dispatch] = useStore();

  const exercise = async (cid: ContractId<T>, argument: C) => {
    setLoading(true);
    await runExercise(store, dispatch, choice, cid, argument);
    setLoading(false);
  }
  return [exercise, loading];
}

export const usePseudoExerciseByKey = <T, C>(choice: Choice<T, C>): [(key: Query<T>, argument: C) => Promise<void>, boolean] => {
  const [loading, setLoading] = useState(false);
  const [store, dispatch] = useStore();

  const exercise = async (key: Query<T>, argument: C) => {
    setLoading(true);
    await runPseudoExerciseByKey(store, dispatch, choice, key, argument);
    setLoading(false);
  }
  return [exercise, loading];
}

export const useParty = () => {
  const [store] = useStore();
  return store.credentials.party;
}

export const useReload = () => {
  const [store, dispatch] = useStore();
  return () => reload(store, dispatch);
}
