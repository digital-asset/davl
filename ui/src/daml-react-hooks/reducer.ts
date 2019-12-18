import { Template } from "@digitalasset/daml-json-types";
import { CreateEvent, Query, Event } from '@digitalasset/daml-ledger-fetch';
import * as LedgerStore from './ledgerStore';

enum ActionType {
  SetQueryLoading,
  SetQueryResult,
  UpdateQueryResult,
  SetFetchByKeyLoading,
  SetFetchByKeyResult,
}

type SetQueryLoadingAction<T extends object> = {
  type: typeof ActionType.SetQueryLoading;
  template: Template<T>;
  query: Query<T>;
}

type SetQueryResultAction<T extends object> = {
  type: typeof ActionType.SetQueryResult;
  template: Template<T>;
  query: Query<T>;
  contracts: CreateEvent<T>[];
}

type UpdateQueryResultAction<T extends object> = {
  type: typeof ActionType.UpdateQueryResult;
  template: Template<T>;
  query: Query<T>;
  events: Event<T>[];
}

type SetFetchByKeyLoadingAction<T extends object, K> = {
  type: typeof ActionType.SetFetchByKeyLoading;
  template: Template<T, K>;
  key: K;
}

type SetFetchByKeyResultAction<T extends object, K> = {
  type: typeof ActionType.SetFetchByKeyResult;
  template: Template<T, K>;
  key: K;
  contract: CreateEvent<T, K> | null;
}

export type Action =
  | SetQueryLoadingAction<object>
  | SetQueryResultAction<object>
  | UpdateQueryResultAction<object>
  | SetFetchByKeyLoadingAction<object, unknown>
  | SetFetchByKeyResultAction<object, unknown>

export const setQueryLoading = <T extends object>(template: Template<T>, query: Query<T>): SetQueryLoadingAction<T> => ({
  type: ActionType.SetQueryLoading,
  template,
  query,
});

export const setQueryResult = <T extends object>(template: Template<T>, query: Query<T>, contracts: CreateEvent<T>[]): SetQueryResultAction<T> => ({
  type: ActionType.SetQueryResult,
  template,
  query,
  contracts,
});

export const updateQueryResult = <T extends object>(template: Template<T>, query: Query<T>, events: Event<T>[]): UpdateQueryResultAction<T> => ({
  type: ActionType.UpdateQueryResult,
  template,
  query,
  events,
});

export const setFetchByKeyLoading = <T extends object, K>(template: Template<T, K>, key: K): SetFetchByKeyLoadingAction<T, K> => ({
  type: ActionType.SetFetchByKeyLoading,
  template,
  key,
});

export const setFetchByKeyResult = <T extends object, K>(template: Template<T, K>, key: K, contract: CreateEvent<T, K> | null): SetFetchByKeyResultAction<T, K> => ({
  type: ActionType.SetFetchByKeyResult,
  template,
  key,
  contract,
});

export const reducer = (ledgerStore: LedgerStore.Store, action: Action): LedgerStore.Store => {
  switch (action.type) {
    case ActionType.SetQueryLoading: {
      return LedgerStore.setQueryLoading(ledgerStore, action.template, action.query);
    }
    case ActionType.SetQueryResult: {
      return LedgerStore.setQueryResult(ledgerStore, action.template, action.query, action.contracts);
    }
    case ActionType.UpdateQueryResult: {
      return LedgerStore.updateQueryResult(ledgerStore, action.template, action.query, action.events);
    }
    case ActionType.SetFetchByKeyLoading: {
      return LedgerStore.setFetchByKeyLoading(ledgerStore, action.template, action.key);
    }
    case ActionType.SetFetchByKeyResult: {
      return LedgerStore.setFetchByKeyResult(ledgerStore, action.template, action.key, action.contract);
    }
  }
}
