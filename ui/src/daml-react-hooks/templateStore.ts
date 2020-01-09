import * as immutable from 'immutable';
import { CreateEvent, Query } from '@digitalasset/daml-ledger-fetch';

export type QueryResult<T extends object, K> = {
  contracts: CreateEvent<T, K>[];
  loading: boolean;
}

export type Store<T extends object, K> = {
  queryResults: immutable.Map<Query<T>, QueryResult<T, K>>;
}

export const emptyQueryResult = <T extends object, K>(): QueryResult<T, K> => ({
  contracts: [],
  loading: false,
});

export const empty = <T extends object, K>(): Store<T, K> => ({
  queryResults: immutable.Map(),
});

export const setAllLoading = <T extends object, K>(store: Store<T, K>): Store<T, K> => ({
  queryResults: store.queryResults.map((res) => ({...res, loading: true})),
})

export const setQueryLoading = <T extends object, K>(store: Store<T, K>, query: Query<T>): Store<T, K> => ({
  ...store,
  queryResults: store.queryResults.update(query, (res = emptyQueryResult()) => ({...res, loading: true})),
})

export const setQueryResult = <T extends object, K>(store: Store<T, K>, query: Query<T>, contracts: CreateEvent<T, K>[]): Store<T, K> => ({
  ...store,
  queryResults: store.queryResults.set(query, {contracts, loading: false})
});
