import { Template } from "@digitalasset/daml-json-types";
import { CreateEvent, Query } from '@digitalasset/daml-ledger-fetch';
import * as LedgerStore from './ledgerStore';

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
  contracts: CreateEvent<T>[];
}

export type Action = SetQueryLoadingAction<object> | SetQueryResultAction<object>;

export const setQueryLoading = <T>(template: Template<T>, query: Query<T>): SetQueryLoadingAction<T> => ({
  type: SET_QUERY_LOADING,
  template,
  query,
});

export const setQueryResult = <T>(template: Template<T>, query: Query<T>, contracts: CreateEvent<T>[]): SetQueryResultAction<T> => ({
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
