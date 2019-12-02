import { Template, Query, Contract, Choice, ContractId, lookupTemplate } from "@digitalasset/daml-json-types";
import { Event } from "./ledger";
import { useEffect, useMemo, useState, useContext } from "react";
import * as LedgerStore from './ledgerStore';
import * as TemplateStore from './templateStore';
import { setQueryLoading, setQueryResult } from "./reducer";
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
  return state.ledger.party;
}

const loadQuery = async <T extends {}>(state: DamlLedgerState, template: Template<T>, query: Query<T>) => {
  state.dispatch(setQueryLoading(template, query));
  const contracts = await state.ledger.query(template, query);
  state.dispatch(setQueryResult(template, query, contracts));
}

const emptyQueryFactory = <T extends {}>(): Query<T> => ({} as Query<T>);

export type QueryResult<T> = {
  contracts: Contract<T>[];
  loading: boolean;
}

/// React Hook for a query against the `/contracts/search` endpoint of the JSON API.
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export const useQuery = <T>(template: Template<T>, queryFactory: () => Query<T> = emptyQueryFactory, queryDeps?: readonly any[]): QueryResult<T> => {
  const state = useDamlState();
  const query = useMemo(queryFactory, queryDeps);
  const contracts = LedgerStore.getQueryResult(state.store, template, query);
  useEffect(() => {
    if (contracts === undefined) {
      // eslint-disable-next-line @typescript-eslint/no-floating-promises
      loadQuery(state, template, query);
    }
  }, [state, template, query, contracts]);
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

const reloadTemplate = async <T extends {}>(state: DamlLedgerState, template: Template<T>) => {
  const templateStore = state.store.templateStores.get(template) as TemplateStore.Store<T> | undefined;
  if (templateStore) {
    const queries: Query<T>[] = Array.from(templateStore.queryResults.keys());
    await Promise.all(queries.map(async (query) => {
      await loadQuery(state, template, query);
    }));
  }
}

const reloadEvents = async (state: DamlLedgerState, events: Event[]) => {
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
export const useExercise = <T, C>(choice: Choice<T, C>): [(cid: ContractId<T>, argument: C) => Promise<void>, boolean] => {
  const [loading, setLoading] = useState(false);
  const state = useDamlState();

  const exercise = async (cid: ContractId<T>, argument: C) => {
    setLoading(true);
    const events = await state.ledger.exercise(choice, cid, argument);
    setLoading(false);
    // NOTE(MH): We want to signal the UI that the exercise is finished while
    // were still updating the affected templates "in the backgound".
    // eslint-disable-next-line @typescript-eslint/no-floating-promises
    reloadEvents(state, events);
  }
  return [exercise, loading];
}

/// React Hook that returns a function to exercise a choice and a boolean
/// indicator whether the exercise is currently running.
export const usePseudoExerciseByKey = <T, C>(choice: Choice<T, C>): [(key: Query<T>, argument: C) => Promise<void>, boolean] => {
  const [loading, setLoading] = useState(false);
  const state = useDamlState();

  const exercise = async (key: Query<T>, argument: C) => {
    setLoading(true);
    const events = await state.ledger.pseudoExerciseByKey(choice, key, argument);
    setLoading(false);
    // NOTE(MH): We want to signal the UI that the exercise is finished while
    // were still updating the affected templates "in the backgound".
    // eslint-disable-next-line @typescript-eslint/no-floating-promises
    reloadEvents(state, events);
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
