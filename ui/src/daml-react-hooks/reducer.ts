import { Template } from "@digitalasset/daml-json-types";
import { CreateEvent, Query } from '@digitalasset/daml-ledger-fetch';
import * as LedgerStore from './ledgerStore';

const SET_QUERY_LOADING = 'SET_QUERY_LOADING';
const SET_QUERY_RESULT = 'SET_QUERY_RESULT';
const SET_FETCH_BY_KEY_LOADING = 'SET_FETCH_BY_KEY_LOADING';
const SET_FETCH_BY_KEY_RESULT = 'SET_FETCH_BY_KEY_RESULT';

type SetQueryLoadingAction<T extends object> = {
  type: typeof SET_QUERY_LOADING;
  template: Template<T>;
  query: Query<T>;
}

type SetQueryResultAction<T extends object> = {
  type: typeof SET_QUERY_RESULT;
  template: Template<T>;
  query: Query<T>;
  contracts: CreateEvent<T>[];
}

type SetFetchByKeyLoadingAction<T extends object, K> = {
  type: typeof SET_FETCH_BY_KEY_LOADING;
  template: Template<T, K>;
  key: K;
}

type SetFetchByKeyResultAction<T extends object, K> = {
  type: typeof SET_FETCH_BY_KEY_RESULT;
  template: Template<T, K>;
  key: K;
  contract: CreateEvent<T, K> | null;
}

export type Action =
  | SetQueryLoadingAction<object>
  | SetQueryResultAction<object>
  | SetFetchByKeyLoadingAction<object, unknown>
  | SetFetchByKeyResultAction<object, unknown>

export const setQueryLoading = <T extends object>(template: Template<T>, query: Query<T>): SetQueryLoadingAction<T> => ({
  type: SET_QUERY_LOADING,
  template,
  query,
});

export const setQueryResult = <T extends object>(template: Template<T>, query: Query<T>, contracts: CreateEvent<T>[]): SetQueryResultAction<T> => ({
  type: SET_QUERY_RESULT,
  template,
  query,
  contracts,
});

export const setFetchByKeyLoading = <T extends object, K>(template: Template<T, K>, key: K): SetFetchByKeyLoadingAction<T, K> => ({
  type: SET_FETCH_BY_KEY_LOADING,
  template,
  key,
});

export const setFetchByKeyResult = <T extends object, K>(template: Template<T, K>, key: K, contract: CreateEvent<T, K> | null): SetFetchByKeyResultAction<T, K> => ({
  type: SET_FETCH_BY_KEY_RESULT,
  template,
  key,
  contract,
});

export const reducer = (ledgerStore: LedgerStore.Store, action: Action): LedgerStore.Store => {
  switch (action.type) {
    case SET_QUERY_LOADING: {
      return LedgerStore.setQueryLoading(ledgerStore, action.template, action.query);
    }
    case SET_QUERY_RESULT: {
      return LedgerStore.setQueryResult(ledgerStore, action.template, action.query, action.contracts);
    }
    case SET_FETCH_BY_KEY_LOADING: {
      return LedgerStore.setFetchByKeyLoading(ledgerStore, action.template, action.key);
    }
    case SET_FETCH_BY_KEY_RESULT: {
      return LedgerStore.setFetchByKeyResult(ledgerStore, action.template, action.key, action.contract);
    }
  }
}
