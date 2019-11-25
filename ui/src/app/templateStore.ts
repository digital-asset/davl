import * as immutable from 'immutable';
import { Query, Contract } from '@digitalasset/daml-json-types';

export type QueryResult<T> = {
  contracts: Contract<T>[];
  loading: boolean;
}

export type Store<T> = {
  queryResults: immutable.Map<Query<T>, QueryResult<T>>;
}

export const emptyQueryResult = <T>(): QueryResult<T> => ({
  contracts: [],
  loading: false,
});

export const empty = <T>(): Store<T> => ({
  queryResults: immutable.Map(),
});

export const setAllLoading = <T>(store: Store<T>): Store<T> => ({
  queryResults: store.queryResults.map((res) => ({...res, loading: true})),
})

export const setQueryLoading = <T>(store: Store<T>, query: Query<T>): Store<T> => ({
  ...store,
  queryResults: store.queryResults.update(query, (res = emptyQueryResult()) => ({...res, loading: true})),
})

export const setQueryResult = <T>(store: Store<T>, query: Query<T>, contracts: Contract<T>[]): Store<T> => ({
  ...store,
  queryResults: store.queryResults.set(query, {contracts, loading: false})
});
