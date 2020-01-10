import {ContractId, registerTemplate} from '@digitalasset/daml-json-types'
import {Event} from '@digitalasset/daml-ledger-fetch'
import * as jtv from '@mojotech/json-type-validation'
import {configure, mount} from 'enzyme'
import Adapter from 'enzyme-adapter-react-16'
import React from 'react'
import DamlLedger from '../daml-react-hooks/DamlLedger'
import Credentials from './credentials'
import {updateEvents, useDamlState} from './hooks'
import {setQueryResult} from "./reducer"

configure({ adapter: new Adapter()})

// mock data
const dummyCredentials : Credentials = {party: 'noparty', token: 'noledger'}
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
  value: {value1 : string, value2: number}
}

const createdEvent = (cid: ContractId<T>, argument: T = payload) : Event<T> => {
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

const archivedEvent = (cid: ContractId<T>) : Event<T> => {
  return(
    {archived: { templateId: templateId
               , contractId: cid
               }
    }
  )
}


// tests
type Props = {
  events: Event<T>[]
}
const HookWrapper : React.FC<Props> = ({events}) => {
  const state = useDamlState()
  const init = () => {
    state.dispatch(setQueryResult(template, query, []))
  }

  const go = () => {
    updateEvents(state, events)
  }

  const result : number | undefined = state.store.templateStores.get(template)?.queryResults.get(query)?.contracts.length
  return(
    <div>
      <button title='init' onClick={init} />
      <button title='button' onClick={go} />
      <span title='result'>{result}</span>
    </div>
  )
}

describe("DAML hooks", () => {
  registerTemplate(template)
  it("no events result in unchanged state", () => {
    const wrapper = mount (
        <DamlLedger credentials={dummyCredentials}>
          <HookWrapper events={[]}>
          </HookWrapper>
        </DamlLedger>
    )
    wrapper.find({title: 'init'}).invoke('onClick')()
    wrapper.find({title: 'button'}).invoke('onClick')()
    expect(wrapper.find({title: 'result'}).text()).toEqual('0')
    wrapper.unmount()
  })

  it("adding one event", () => {
    const wrapper = mount(
        <DamlLedger credentials={dummyCredentials}>
          <HookWrapper events={[createdEvent('0#0')]}>
          </HookWrapper>
        </DamlLedger>
    )
    wrapper.find({title: 'init'}).invoke('onClick')()
    wrapper.find({title: 'button'}).invoke('onClick')()
    expect(wrapper.find({title: 'result'}).text()).toEqual('1')
    wrapper.unmount()
  })

  it("adding three events", () => {
    const wrapper = mount(
        <DamlLedger credentials={dummyCredentials}>
          <HookWrapper events={[createdEvent('0#0'), createdEvent('0#1'), createdEvent('0#2')]}>
          </HookWrapper>
        </DamlLedger>
    )
    wrapper.find({title: 'init'}).invoke('onClick')()
    wrapper.find({title: 'button'}).invoke('onClick')()
    expect(wrapper.find({title: 'result'}).text()).toEqual('3')
    wrapper.unmount()
  })

  it("adding two events and archiving one", () => {
    const wrapper = mount(
        <DamlLedger credentials={dummyCredentials}>
          <HookWrapper events={[createdEvent('0#0'), createdEvent('0#1'), archivedEvent('0#0')]}>
          </HookWrapper>
        </DamlLedger>
    )
    wrapper.find({title: 'init'}).invoke('onClick')()
    wrapper.find({title: 'button'}).invoke('onClick')()
    expect(wrapper.find({title: 'result'}).text()).toEqual('1')
    wrapper.unmount()
  })

  it("archiving a non-existant contract", () => {
    const wrapper = mount(
        <DamlLedger credentials={dummyCredentials}>
          <HookWrapper events={[createdEvent('0#0'), createdEvent('0#1'), archivedEvent('0#2')]}>
          </HookWrapper>
        </DamlLedger>
    )
    wrapper.find({title: 'init'}).invoke('onClick')()
    wrapper.find({title: 'button'}).invoke('onClick')()
    expect(wrapper.find({title: 'result'}).text()).toEqual('2')
    wrapper.unmount()
  })

  it("adding an event that doesn't match the query", () => {
    const wrapper = mount(
        <DamlLedger credentials={dummyCredentials}>
          <HookWrapper events={[createdEvent('0#0', {value : {value1: 'something else', value2: 1}})]}>
          </HookWrapper>
        </DamlLedger>
    )
    wrapper.find({title: 'init'}).invoke('onClick')()
    wrapper.find({title: 'button'}).invoke('onClick')()
    expect(wrapper.find({title: 'result'}).text()).toEqual('0')
    wrapper.unmount()
  })
});
