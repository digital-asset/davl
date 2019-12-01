/* eslint-disable @typescript-eslint/no-namespace */
import { Template, Query, Contract, Choice, ContractId, lookupTemplate } from "@digitalasset/daml-json-types";
import * as root from './rootReducer';
import { useSelector, useDispatch } from "react-redux";
import { AppThunk, AppDispatch } from "./store";
import Ledger, { Event } from "../ledger/ledger";
import Credentials from "../ledger/credentials";
import * as QueryStore from './query';
import { useEffect, useMemo, useState } from "react";
import * as LedgerStore from './ledgerStore';
import * as TemplateStore from './templateStore';

type State = LedgerStore.Store | null;

const getLedgerStore = (state: root.RootState): LedgerStore.Store => {
  if (!state.daml) {
    throw Error(`getLedgerStore before ${START}`);
  }
  return state.daml;
}

const START = 'daml/START';
const STOP = 'daml/STOP';
const SET_QUERY_LOADING = 'daml/SET_QUERY_LOADING';
const SET_QUERY_RESULT = 'daml/SET_QUERY_RESULT';

type StartAction = {
  type: typeof START;
  credentials: Credentials;
}

type StopAction = {
  type: typeof STOP;
}

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

type Action = StartAction | StopAction | SetQueryLoadingAction<object> | SetQueryResultAction<object>;

export const start = (credentials: Credentials): StartAction => ({
  type: START,
  credentials,
});

export const stop = (): StopAction => ({
  type: STOP,
});

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

const loadQuery = <T>(template: Template<T>, query: Query<T>): AppThunk => async (dispatch, getState) => {
  const ledgerStore = getLedgerStore(getState());
  dispatch(setQueryLoading(template, query));
  const ledger = new Ledger(ledgerStore.credentials);
  const contracts = await ledger.query(template, query);
  dispatch(setQueryResult(template, query, contracts));
}

export const reloadTemplate = <T extends {}>(template: Template<T>): AppThunk => async (dispatch, getState) => {
  const ledgerStore = getLedgerStore(getState());
  const templateStore = ledgerStore.templateStores.get(template) as TemplateStore.Store<T> | undefined;
  if (templateStore) {
    const queries: Query<T>[] = Array.from(templateStore.queryResults.keys());
    await Promise.all(queries.map(async (query) => {
      await dispatch(loadQuery(template, query));
    }));
  }
}

const reloadForEvents = (events: Event[]): AppThunk => async (dispatch) => {
  // TODO(MH): This is a sledge hammer approach. We completely reload every
  // single template that has been touched by the events. A future optimization
  // would be to remove the archived templates from their tables and add the
  // created templates wherever they match.
  const templates = new Set(events.map((event) =>
    lookupTemplate('created' in event ? event.created.templateId : event.archived.templateId)
  ));
  await Promise.all(Array.from(templates).map((template) => dispatch(reloadTemplate(template))));
}

const runExercise = <T, C>(choice: Choice<T, C>, cid: ContractId<T>, argument: C): AppThunk => async (dispatch, getState) => {
  const ledgerStore = getLedgerStore(getState());
  const ledger = new Ledger(ledgerStore.credentials);
  const events = await ledger.exercise(choice, cid, argument);
  // NOTE(MH): We want to signal the UI that the exercise is finished while
  // were still updating the affected templates "in the backgound".
  // eslint-disable-next-line @typescript-eslint/no-floating-promises
  dispatch(reloadForEvents(events));
}

const runPseudoExerciseByKey = <T, C>(choice: Choice<T, C>, key: Query<T>, argument: C): AppThunk => async (dispatch, getState) => {
  const ledgerStore = getLedgerStore(getState());
  const ledger = new Ledger(ledgerStore.credentials);
  const events = await ledger.pseudoExerciseByKey(choice, key, argument);
  // NOTE(MH): We want to signal the UI that the exercise is finished while
  // were still updating the affected templates "in the backgound".
  // eslint-disable-next-line @typescript-eslint/no-floating-promises
  dispatch(reloadForEvents(events));
}


const initialState: State = null;

export const reducer = (state = initialState as State, action: Action): State => {
  switch (action.type) {
    case START: {
      return LedgerStore.empty(action.credentials);
    }
    case STOP: {
      return initialState;
    }
    case SET_QUERY_LOADING: {
      if (!state) {
        throw Error(`${action.type} before ${START}`);
      }
      return LedgerStore.setQueryLoading(state, action.template, action.query);
    }
    case SET_QUERY_RESULT: {
      if (!state) {
        throw Error(`${action.type} before ${START}`);
      }
      return LedgerStore.setQueryResult(state, action.template, action.query, action.contracts);
    }
  }
  return state;
}

const emptyQueryFactory = <T extends {}>(): Query<T> => ({} as Query<T>);

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export const useQuery = <T>(template: Template<T>, queryFactory: () => Query<T> = emptyQueryFactory, queryDeps?: readonly any[]): QueryStore.Entry<T> => {
  const dispatch = useDispatch();
  const query = useMemo(queryFactory, queryDeps);
  const contracts = useSelector((state: root.RootState) => LedgerStore.getQueryResult(getLedgerStore(state), template, query));
  useEffect(() => {
    if (contracts === undefined) {
      dispatch(loadQuery(template, query));
    }
  }, [dispatch, template, query, contracts]);
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

export const useExercise = <T, C>(choice: Choice<T, C>, additional?: Template<object>[]): [(cid: ContractId<T>, argument: C) => Promise<void>, boolean] => {
  const [loading, setLoading] = useState(false);
  const dispatch: AppDispatch = useDispatch();

  const exercise = async (cid: ContractId<T>, argument: C) => {
    setLoading(true);
    await dispatch(runExercise(choice, cid, argument));
    setLoading(false);
  }
  return [exercise, loading];
}

export const usePseudoExerciseByKey = <T, C>(choice: Choice<T, C>, additional?: Template<object>[]): [(key: Query<T>, argument: C) => Promise<void>, boolean] => {
  const [loading, setLoading] = useState(false);
  const dispatch: AppDispatch = useDispatch();

  const exercise = async (key: Query<T>, argument: C) => {
    setLoading(true);
    await dispatch(runPseudoExerciseByKey(choice, key, argument));
    setLoading(false);
  }
  return [exercise, loading];
}
