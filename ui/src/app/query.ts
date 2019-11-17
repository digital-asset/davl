import * as immutable from "immutable";
import { Query, Contract, Template } from "@digitalasset/daml-json-types";

export type Entry<T> = immutable.Map<Query<T>, Contract<T>[]>;

export type Store = immutable.Map<Template<object>, Entry<object>>;

export const empty: Store = immutable.Map();

export const get = <T extends {}>(store: Store, template: Template<T>, query: Query<T>): Contract<T>[] | undefined => {
  const entry = store.get(template);
  return entry ? entry.get(query) as (Contract<T>[] | undefined) : undefined;
}

export const set = <T extends {}>(store: Store, template: Template<T>, query: Query<T>, contracts: Contract<T>[]): Store => {
  let entry = store.get(template) as (Entry<T> | undefined) || immutable.Map();
  entry = entry.set(query, contracts);
  store = store.set(template, entry);
  return store;
}
