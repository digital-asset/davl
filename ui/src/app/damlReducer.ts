/* eslint-disable @typescript-eslint/no-namespace */
import { Template, Query, Contract } from "@digitalasset/daml-json-types";
import * as root from './rootReducer';
import { useSelector, useDispatch } from "react-redux";
import { AppThunk } from "./store";
import Ledger from "../ledger/ledger";
import Credentials from "../ledger/credentials";
import * as QueryStore from './query';
import { useEffect, useMemo } from "react";

type DamlStore = {
  credentials?: Credentials;
  queryStore: QueryStore.Store;
}

type State = DamlStore;

const START = 'daml/START';
const STOP = 'daml/STOP';
const SET_QUERY = 'daml/SET_QUERY';
const SET_ALL_QUERIES = 'daml/SET_ALL_QUERIES';

type StartAction = {
  type: typeof START;
  credentials: Credentials;
}

type StopAction = {
  type: typeof STOP;
}

type SetQueryAction<T> = {
  type: typeof SET_QUERY;
  template: Template<T>;
  query: Query<T>;
  contracts: Contract<T>[];
}

type SetAllQueriesAction<T> = {
  type: typeof SET_ALL_QUERIES;
  template: Template<T>;
  entries: QueryStore.Entries<T>;
}

type Action = StartAction | StopAction | SetQueryAction<object> | SetAllQueriesAction<object>;

export const start = (credentials: Credentials): StartAction => ({
  type: START,
  credentials,
});

export const stop = (): StopAction => ({
  type: STOP,
});

const setQuery = <T extends {}>(template: Template<T>, query: Query<T>, contracts: Contract<T>[]): SetQueryAction<T> => ({
  type: SET_QUERY,
  template,
  query,
  contracts,
});

const setAllQueries = <T extends {}>(template: Template<T>, entries: QueryStore.Entries<T>): SetAllQueriesAction<T> => ({
  type: SET_ALL_QUERIES,
  template,
  entries,
});

const loadQuery = <T>(template: Template<T>, query: Query<T>): AppThunk => async (dispatch, getState) => {
  const credentials = getState().daml.credentials;
  if (!credentials) {
    throw Error("loadQuery called before start");
  }
  const ledger = new Ledger(credentials);
  const contracts = await ledger.query(template, query);
  dispatch(setQuery(template, query, contracts));
}

export const reloadTemplate = <T extends {}>(template: Template<T>): AppThunk => async (dispatch, getState) => {
  const state = getState();
  const credentials = state.daml.credentials;
  if (!credentials) {
    throw Error("loadQuery called before start");
  }
  const ledger = new Ledger(credentials);
  let entries: QueryStore.Entries<T> | undefined = QueryStore.getEntries(state.daml.queryStore, template);
  if (entries) {
    entries = entries.map((entry) => ({...entry, loading: true}));
    dispatch(setAllQueries(template, entries));
    const queries: Query<T>[] = Array.from(entries.keys());
    await Promise.all(queries.map(async (query) => {
      const contracts = await ledger.query(template, query);
      dispatch(setQuery(template, query, contracts));
    }));
  }
}


const initialState: State = {queryStore: QueryStore.empty};

export const reducer = (state = initialState, action: Action): State => {
  switch (action.type) {
    case START:
      return {...initialState, credentials: action.credentials};
    case STOP:
      return initialState;
    case SET_QUERY:
      return {
        ...state,
        queryStore: QueryStore.set(state.queryStore, action.template, action.query, action.contracts),
      };
    case SET_ALL_QUERIES:
      return {
        ...state,
        queryStore: QueryStore.setAll(state.queryStore, action.template, action.entries),
      }
    default:
      return state;
  }
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export const useQuery = <T extends {}>(template: Template<T>, queryFactory: () => Query<T>, queryDeps: readonly any[] | undefined): QueryStore.Entry<T> => {
  const dispatch = useDispatch();
  const query = useMemo(queryFactory, queryDeps)
  const contracts = useSelector((state: root.RootState) => QueryStore.getEntry(state.daml.queryStore, template, query));
  useEffect(() => {
    if (contracts === undefined) {
      dispatch(loadQuery(template, query));
    }
  }, [dispatch, template, query, contracts]);
  return contracts || {loading: true, contracts: []};
}
