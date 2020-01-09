import * as immutable from 'immutable';
import { CreateEvent, Query } from '@digitalasset/daml-ledger-fetch';

export type QueryResult<T extends object> = {
  contracts: CreateEvent<T>[];
  loading: boolean;
}

export type Store<T extends object> = {
  queryResults: immutable.Map<Query<T>, QueryResult<T>>;
}

export const emptyQueryResult = <T extends object>(): QueryResult<T> => ({
  contracts: [],
  loading: false,
});

export const empty = <T extends object>(): Store<T> => ({
  queryResults: immutable.Map(),
});

export const setAllLoading = <T extends object>(store: Store<T>): Store<T> => ({
  queryResults: store.queryResults.map((res) => ({...res, loading: true})),
})

export const setQueryLoading = <T extends object>(store: Store<T>, query: Query<T>): Store<T> => ({
  ...store,
  queryResults: store.queryResults.update(query, (res = emptyQueryResult()) => ({...res, loading: true})),
})

export const setQueryResult = <T extends object>(store: Store<T>, query: Query<T>, contracts: CreateEvent<T>[]): Store<T> => ({
  ...store,
  queryResults: store.queryResults.set(query, {contracts, loading: false})
});
