import * as immutable from "immutable";
import { Query, Contract, Template } from "@digitalasset/daml-json-types";

export type Entry<T> = {
  loading: boolean;
  contracts: Contract<T>[];
}

export type KeyEntry<T> = {
  loading: boolean;
  contract: Contract<T> | null;
}

export type Entries<T> = immutable.Map<Query<T>, Entry<T>>;

export type Store = immutable.Map<Template<object>, Entries<object>>;

export const empty: Store = immutable.Map();

export const getEntries = <T extends {}>(store: Store, template: Template<T>): Entries<T> | undefined =>
  (store.get(template) as Entries<T> | undefined);

export const getEntry = <T extends {}>(store: Store, template: Template<T>, query: Query<T>): Entry<T> | undefined => {
  const entries = getEntries(store, template);
  return entries ? entries.get(query) : undefined;
}

export const set = <T extends {}>(store: Store, template: Template<T>, query: Query<T>, contracts: Contract<T>[]): Store => {
  let entries = store.get(template) as (Entries<T> | undefined) || immutable.Map();
  entries = entries.set(query, {loading: false, contracts});
  store = store.set(template, entries);
  return store;
}

export const setAll = <T extends {}>(store: Store, template: Template<T>, entries: Entries<T>): Store => {
  store = store.set(template, entries);
  return store;
}
