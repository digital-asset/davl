import * as immutable from 'immutable';
import { Event, Query, CreateEvent} from '@digitalasset/daml-ledger-fetch';
import { ContractId } from '@digitalasset/daml-json-types'

export type QueryResult<T extends object, K> = {
  contracts: CreateEvent<T, K>[];
  loading: boolean;
}

export type FetchResult<T extends object, K> = {
  contract: CreateEvent<T, K> | null;
  loading: boolean;
}

export type Store<T extends object, K> = {
  queryResults: immutable.Map<Query<T>, QueryResult<T, K>>;
  fetchByKeyResults: immutable.Map<K, FetchResult<T, K>>;
}

export const emptyQueryResult = <T extends object, K>(): QueryResult<T, K> => ({
  contracts: [],
  loading: false,
});

export const emptyFetchResult = <T extends object, K>(): FetchResult<T, K> => ({
  contract: null,
  loading: false,
});

export const empty = <T extends object, K>(): Store<T, K> => ({
  queryResults: immutable.Map(),
  fetchByKeyResults: immutable.Map(),
});

export const setAllLoading = <T extends object, K>(store: Store<T, K>): Store<T, K> => ({
  queryResults: store.queryResults.map((res) => ({...res, loading: true})),
  fetchByKeyResults: store.fetchByKeyResults.map((res) => ({...res, laoding: true})),
});

export const setQueryLoading = <T extends object, K>(store: Store<T, K>, query: Query<T>): Store<T, K> => ({
  ...store,
  queryResults: store.queryResults.update(query, (res = emptyQueryResult()) => ({...res, loading: true})),
});

export const setQueryResult = <T extends object, K>(store: Store<T, K>, query: Query<T>, contracts: CreateEvent<T, K>[]): Store<T, K> => ({
  ...store,
  queryResults: store.queryResults.set(query, {contracts, loading: false})
});

export const updateQueryResult = <T extends object, K>(store: Store<T, K>, query: Query<T>, events: Event<T>[]): Store<T, K> => ({
  ...store,
  queryResults: store.queryResults.update(query, (res = emptyQueryResult()) => {
    // partition the events into archived contract ids and created events that match the query.
    const archivedAndCreated: [ContractId<T>[], CreateEvent<T, K>[]] = events.reduce<[ContractId<T>[], CreateEvent<T, K>[]]>
      ((acc, event : Event<T>) => {
          return ('created' in event ? [acc[0], eventMatchesQuery(event.created.payload, query) ? acc[1].concat(event.created as CreateEvent<T, K>) : acc[1]]
                                      : [acc[0].concat(event.archived.contractId), acc[1]])
        }
      , [[], []])

    // append the newly created events that match the query and drop the archived ones.
    const updatedContracts = res.contracts.concat(archivedAndCreated[1])
                                          .filter((value : CreateEvent<T, K>) => !archivedAndCreated[0].includes(value.contractId))
    return {contracts: updatedContracts, loading: false}
  })
});

export const eventMatchesQuery= <T>(payload: T, query: Query<T>) : boolean => {
  if (typeof payload === "object" && typeof query === "object") {
    const keys = Object.keys(query) as (keyof Query<T> & keyof T)[]
    return keys.reduce<boolean>((acc, k) => {
      if (k in payload)
        return eventMatchesQuery(payload[k], query[k] as any) && acc
      else
        return false
    }, true)
  } else if (typeof payload === typeof query) {
      return payload === query
  } else
      return false
}


export const setFetchByKeyLoading = <T extends object, K>(store: Store<T, K>, key: K): Store<T, K> => ({
  ...store,
  fetchByKeyResults: store.fetchByKeyResults.update(key, (res = emptyFetchResult()) => ({...res, loading: true})),
});

export const setFetchByKeyResult = <T extends object, K>(store: Store<T, K>, key: K, contract: CreateEvent<T, K> | null) => ({
  ...store,
  fetchByKeyResults: store.fetchByKeyResults.set(key, {contract, loading: false}),
});

