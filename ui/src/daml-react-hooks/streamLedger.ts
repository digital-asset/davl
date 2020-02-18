import * as jtv from '@mojotech/json-type-validation'
import Ledger, { Query, Event, CreateEvent } from '@daml/ledger';
import { Party, Template, List, Text, ContractId } from '@daml/types';
import { EventEmitter } from 'events';
import WebSocket from 'isomorphic-ws';

const decodeCreateEvent = <T extends object, K, I extends string>(template: Template<T, K, I>): jtv.Decoder<CreateEvent<T, K, I>> => jtv.object({
  templateId: jtv.constant(template.templateId),
  contractId: ContractId(template).decoder(),
  signatories: List(Party).decoder(),
  observers: List(Party).decoder(),
  agreementText: Text.decoder(),
  key: template.keyDecoder(),
  payload: template.decoder(),
});

const decodeStreamEvent = <T extends object, K, I extends string>(template: Template<T, K, I>): jtv.Decoder<Event<T, K, I>> => jtv.oneOf<Event<T, K, I>>(
  jtv.object({created: decodeCreateEvent(template)}),
  jtv.object({archived: ContractId(template).decoder().map(contractId => ({
    templateId: template.templateId,
    contractId,
  }))}),
);

function isRecordWith<Field extends string>(field: Field, x: unknown): x is Record<Field, unknown> {
  return typeof x === "object" && x !== null && field in x;
}

export type StreamCloseEvent = {
  code: number;
  reason: string;
}

export interface Stream<T extends object, K, I extends string, State> {
  on(type: 'change', listener: (state: State, events: readonly Event<T, K, I>[]) => void): void;
  on(type: 'close', listener: (closeEvent: StreamCloseEvent) => void): void;
  off(type: 'change', listener: (state: State, events: readonly Event<T, K, I>[]) => void): void;
  off(type: 'close', listener: (closeEvent: StreamCloseEvent) => void): void;
  close(): void;
}

type LedgerOptions = {
  token: string;
  httpBaseUrl?: string;
  wsBaseUrl?: string;
}

// TODO(MH): Remove this class when we switch to `@daml/ledger-0.13.53`.
export default class StreamLedger extends Ledger {
  private readonly wsBaseUrl: string;

  constructor({token, httpBaseUrl, wsBaseUrl}: LedgerOptions) {
    if (!httpBaseUrl) {
      httpBaseUrl = `${window.location.protocol}//${window.location.host}/`;
    }
    if (!(httpBaseUrl.startsWith('http://') || httpBaseUrl.startsWith('https://'))) {
      throw Error(`Ledger: httpBaseUrl must start with 'http://' or 'https://'. (${httpBaseUrl})`);
    }
    if (!httpBaseUrl.endsWith('/')) {
      throw Error(`Ledger: httpBaseUrl must end with '/'. (${httpBaseUrl})`);
    }
    if (!wsBaseUrl) {
      wsBaseUrl = 'ws' + httpBaseUrl.slice(4);
    }
    if (!(wsBaseUrl.startsWith('ws://') || wsBaseUrl.startsWith('wss://'))) {
      throw Error(`Ledger: wsBaseUrl must start with 'ws://' or 'wss://'. (${wsBaseUrl})`);
    }
    if (!wsBaseUrl.endsWith('/')) {
      throw Error(`Ledger: wsBaseUrl must end with '/'. (${wsBaseUrl})`);
    }

    super(token, httpBaseUrl);
    this.wsBaseUrl = wsBaseUrl;
  }

  private streamSubmit<T extends object, K, I extends string, State>(
    template: Template<T, K, I>,
    endpoint: string,
    request: unknown,
    init: State,
    change: (state: State, events: readonly Event<T, K, I>[]) => State,
  ): Stream<T, K, I, State> {
    const protocols = ['jwt.token.' + this['token'], 'daml.ws.auth'];
    const ws = new WebSocket(this.wsBaseUrl + endpoint, protocols);
    let haveSeenEvents = false;
    let state = init;
    const emitter = new EventEmitter();
    ws.onopen = () => {
      ws.send(JSON.stringify(request));
    };
    ws.onmessage = event => {
      const json: unknown = JSON.parse(event.data.toString());
      if (Array.isArray(json)) {
        const events = jtv.Result.withException(jtv.array(decodeStreamEvent(template)).run(json));
        state = change(state, events);
        haveSeenEvents = true;
        emitter.emit('change', state, events);
      } else if (isRecordWith('heartbeat', json)) {
        // NOTE(MH): If we receive the first heartbeat before any events, then
        // it's very likely nothing in the ACS matches the query. We signal this
        // by pretending we received an empty list of events. This never does
        // any harm.
        if (!haveSeenEvents) {
          haveSeenEvents = true;
          emitter.emit('change', state, []);
        }
      } else if (isRecordWith('warnings', json)) {
        console.warn('Ledger.streamQuery warnings', json);
      } else if (isRecordWith('errors', json)) {
        console.error('Ledger.streamQuery errors', json);
      } else {
        console.error('Ledger.streamQuery unknown message', json);
      }
    };
    // NOTE(MH): We ignore the 'error' event since it is always followed by a
    // 'close' event, which we need to handle anyway.
    ws.onclose = ({code, reason}) => {
      emitter.emit('close', {code, reason});
    }
    // TODO(MH): Make types stricter.
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const on = (type: string, listener: any) => emitter.on(type, listener);
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const off = (type: string, listener: any) => emitter.on(type, listener);
    const close = () => {
      emitter.removeAllListeners();
      ws.close();
    }
    return {on, off, close};
  }

  /**
   * Retrieve a consolidated stream of events for a given template and query.
   * The accumulated state is the current set of active contracts matching the
   * query.
   * When no `query` argument is
   * given, all events visible to the submitting party are returned. When a
   * `query` argument is given, only those create events matching the query are
   * returned. See https://docs.daml.com/json-api/search-query-language.html
   * for a description of the query language.
   */
  streamQuery<T extends object, K, I extends string>(
    template: Template<T, K, I>,
    query?: Query<T>,
  ): Stream<T, K, I, readonly CreateEvent<T, K, I>[]> {
    const request = {templateIds: [template.templateId], query};
    const change = (contracts: readonly CreateEvent<T, K, I>[], events: readonly Event<T, K, I>[]) => {
      const archiveEvents: Set<ContractId<T>> = new Set();
      const createEvents: CreateEvent<T, K, I>[] = [];
      for (const event of events) {
        if ('created' in event) {
          createEvents.push(event.created);
        } else { // i.e. 'archived' in event
          archiveEvents.add(event.archived.contractId);
        }
      }
      return contracts
        .concat(createEvents)
        .filter(contract => !archiveEvents.has(contract.contractId));
    };
    return this.streamSubmit(template, 'contracts/searchForever', request, [], change);
  }
}
