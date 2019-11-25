import * as immutable from 'immutable';
import { Query, Contract, Template } from '@digitalasset/daml-json-types';
import Credentials from '../ledger/credentials';
import * as TemplateStore from './templateStore';

export type Store = {
  credentials: Credentials;
  templateStores: immutable.Map<Template<object>, TemplateStore.Store<object>>;
}

export const empty = (credentials: Credentials): Store => ({
  credentials,
  templateStores: immutable.Map(),
});

export const setTemplateLoading = <T extends {}>(store: Store, template: Template<T>) => ({
  ...store,
  templateStores: store.templateStores.update(template, TemplateStore.setAllLoading)
});

export const getQueryResult = <T extends {}>(store: Store, template: Template<T>, query: Query<T>): TemplateStore.QueryResult<T> | undefined=> {
  const templateStore = store.templateStores.get(template) as TemplateStore.Store<T> | undefined;
  return templateStore === undefined ? undefined : templateStore.queryResults.get(query);
}

export const setQueryLoading = <T extends {}>(store: Store, template: Template<T>, query: Query<T>): Store => ({
  ...store,
  templateStores: store.templateStores.update(template, (templateStore = TemplateStore.empty()) =>
    TemplateStore.setQueryLoading(templateStore, query))
});

export const setQueryResult = <T extends {}>(store: Store, template: Template<T>, query: Query<T>, contracts: Contract<T>[]): Store => ({
  ...store,
  templateStores: store.templateStores.update(template, (templateStore = TemplateStore.empty()) =>
    TemplateStore.setQueryResult(templateStore, query, contracts))
});
