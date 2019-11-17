/* eslint-disable @typescript-eslint/no-namespace */
import { Template, Query, Contract } from "@digitalasset/daml-json-types";
import * as root from './rootReducer';
import { useSelector, useDispatch } from "react-redux";
import { AppThunk } from "./store";
import Ledger from "../ledger/ledger";
import Credentials from "../ledger/credentials";
import * as QueryStore from './query';

type DamlStore = {
  credentials?: Credentials;
  queryStore: QueryStore.Store;
}

type State = DamlStore;

const START = 'daml/START';
const STOP = 'daml/STOP';
const SET_QUERY = 'daml/SET_QUERY';

type StartAction = {
  type: typeof START;
  credentials: Credentials;
}

type StopAction = {
  type: typeof STOP;
}

type SetQueryAction<T> = {
  type: typeof SET_QUERY;
  template: Template<T>;
  query: Query<T>;
  contracts: Contract<T>[];
}

type Action = StartAction | StopAction | SetQueryAction<object>;

export const start = (credentials: Credentials): StartAction => ({
  type: START,
  credentials,
});

export const stop = (): StopAction => ({
  type: STOP,
});

const setQuery = <T extends {}>(template: Template<T>, query: Query<T>, contracts: Contract<T>[]): SetQueryAction<T> => ({
  type: SET_QUERY,
  template,
  query,
  contracts,
});

const loadQuery = <T>(template: Template<T>, query: Query<T>): AppThunk => async (dispatch, getState) => {
  const credentials = getState().daml.credentials;
  if (!credentials) {
    throw Error("loadQuery called before start");
  }
  const ledger = new Ledger(credentials);
  const contracts = await ledger.query(template, query);
  dispatch(setQuery(template, query, contracts));
}


const initialState: State = {queryStore: QueryStore.empty};

export const reducer = (state = initialState, action: Action): State => {
  switch (action.type) {
    case START:
      return {...initialState, credentials: action.credentials};
    case STOP:
      return initialState;
    case SET_QUERY:
      return {
        ...state,
        queryStore: QueryStore.set(state.queryStore, action.template, action.query, action.contracts),
      };
    default:
      return state;
  }
}

export const useQuery = <T extends {}>(template: Template<T>, query: Query<T>): Contract<T>[] => {
  const dispatch = useDispatch();
  const contracts = useSelector((state: root.RootState) => QueryStore.get(state.daml.queryStore, template, query));
  if (contracts === undefined) {
    dispatch(loadQuery(template, query));
    return [];
  } else {
    return contracts;
  }
}
