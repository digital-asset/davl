import {ContractId, registerTemplate} from '@digitalasset/daml-json-types'
import Ledger, {Event} from '@digitalasset/daml-ledger-fetch'
import * as jtv from '@mojotech/json-type-validation'
import {configure, mount} from 'enzyme'
import Adapter from 'enzyme-adapter-react-16'
import React from 'react'
import DamlLedger from '../daml-react-hooks/DamlLedger'
import Credentials from './credentials'
import {updateEvents, useDamlState} from './hooks'
import {setQueryResult, Action, reducer} from "./reducer"
import {DamlLedgerState} from './context'
import * as LedgerStore from './ledgerStore'

configure({ adapter: new Adapter()})

// mock data
const dummyCredentials: Credentials = {party: 'noparty', token: 'noledger'}
const templateId = {  packageId: 'A'
                    , moduleName: 'B'
                    , entityName : 'C'
                    }
const template = {  templateId: templateId
                  , Archive: {  template: () => template
                              , choiceName: 'Archive'
                              , argumentDecoder: jtv.anyJson
                              , resultDecoder: jtv.anyJson
                              }
                  , keyDecoder: jtv.string
                  , decoder: jtv.anyJson
                  }

const query = {value: {value1 : '123'}}
const payload = {value: {value1 : '123', value2: 1}}
const key = "key"

type T={
  value: {value1: string, value2: number}
}

const createdEvent = (cid: ContractId<T>, argument: T = payload): Event<T> => {
  return(
    {created: { templateId: templateId
              , contractId: cid
              , signatories: []
              , observers: []
              , agreementText: ''
              , key: key
              , payload: argument
              }
    }
  )
}

const archivedEvent = (cid: ContractId<T>): Event<T> => {
  return(
    {archived: { templateId: templateId
               , contractId: cid
               }
    }
  )
}

const mockDamlLedgerState = (): DamlLedgerState => {
  const state: DamlLedgerState = {
    store: LedgerStore.setQueryResult(LedgerStore.empty(), template, query, []),
    dispatch: () => {return;},
    party: 'NO_PARTY',
    ledger: new Ledger('NO_TOKEN'),
  };
  state.dispatch = (action: Action) => {
    state.store = reducer(state.store, action);
  }
  return state;
}

describe('daml-react-hooks', () => {
  registerTemplate(template)

  it("no events result in unchanged state", () => {
    const state = mockDamlLedgerState();
    updateEvents(state, [])
    expect(state.store.templateStores.get(template)?.queryResults.get(query)?.contracts).toHaveLength(0);
  });

  it("adding one event", () => {
    const state = mockDamlLedgerState();
    updateEvents(state, [createdEvent('0#0')])
    expect(state.store.templateStores.get(template)?.queryResults.get(query)?.contracts).toHaveLength(1);
  });

  it("adding three events", () => {
    const state = mockDamlLedgerState();
    updateEvents(state, [createdEvent('0#0'), createdEvent('0#1'), createdEvent('0#2')])
    expect(state.store.templateStores.get(template)?.queryResults.get(query)?.contracts).toHaveLength(3);
  })

  it("adding two events and archiving one", () => {
    const state = mockDamlLedgerState();
    updateEvents(state, [createdEvent('0#0'), createdEvent('0#1'), archivedEvent('0#0')])
    expect(state.store.templateStores.get(template)?.queryResults.get(query)?.contracts).toHaveLength(1);
  })

  it("archiving a non-existant contract", () => {
    const state = mockDamlLedgerState();
    updateEvents(state, [createdEvent('0#0'), archivedEvent('0#2')])
    expect(state.store.templateStores.get(template)?.queryResults.get(query)?.contracts).toHaveLength(1);
  })

  it("adding an event that doesn't match the query", () => {
    const state = mockDamlLedgerState();
    updateEvents(state, [createdEvent('0#0', {value : {value1: 'something else', value2: 1}})])
    expect(state.store.templateStores.get(template)?.queryResults.get(query)?.contracts).toHaveLength(0);
  })
});
