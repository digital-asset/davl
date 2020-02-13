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

export type EventStreamCloseEvent = {
  code: number;
  reason: string;
}

export interface EventStream<T extends object, K, I extends string> {
  on(type: 'events', listener: (events: readonly Event<T,K, I>[]) => void): void;
  on(type: 'close', listener: (closeEvent: EventStreamCloseEvent) => void): void;
  off(type: 'events', listener: (events: readonly Event<T,K, I>[]) => void): void;
  off(type: 'close', listener: (closeEvent: EventStreamCloseEvent) => void): void;
  close(): void;
}

// TODO(MH): Move the `streamQuery` method into upstream `Ledger`.
export default class StreamLedger extends Ledger {
  private readonly wsBaseUrl: string;
  constructor(token: string, baseUrl?: string) {
    super(token, baseUrl);
    if (!baseUrl) {
      const protocol = window.location.protocol === 'http:' ? 'ws:' : 'wss:';
      // NOTE(MH): Since the `create-react-app`'s dev server does not properly
      // proxy websockets, we need to connect to the JSON API directly without
      // going through its proxy. This proxy avoiding behaviour is triggered
      // by setting the `REACT_APP_JSON_API_PORT` environment variable in
      // `.env.development`.
      const port = process.env.REACT_APP_JSON_API_PORT;
      const host = port ? `${window.location.hostname}:${port}` : window.location.host;
      this.wsBaseUrl = `${protocol}//${host}/`;
    } else if (!baseUrl.startsWith('http')) {
      throw Error(`The ledger base URL must start with 'http'. (${baseUrl})`);
    } else if (!baseUrl.endsWith('/')) {
      throw Error(`The ledger base URL must end in a '/'. (${baseUrl})`);
    } else {
      this.wsBaseUrl = 'ws' + baseUrl.slice(4);
    }
  }

  streamQuery<T extends object, K, I extends string>(
    template: Template<T, K, I>,
    query?: Query<T>,
  ): EventStream<T, K, I> {
    // TODO(MH): When this moves into the proper `Ledger` class, we should use
    // `this.token` instead of this hack around privacy using `this['token']`.
    const protocols = ['jwt.token.' + this['token'], 'daml.ws.auth'];
    const ws = new WebSocket(this.wsBaseUrl + 'contracts/searchForever', protocols);
    let haveSeenEvents = false;
    const emitter = new EventEmitter();
    ws.onopen = () => {
      const payload = {templateIds: [template.templateId], query};
      ws.send(JSON.stringify(payload));
    };
    ws.onmessage = event => {
      const json: unknown = JSON.parse(event.data.toString());
      if (Array.isArray(json)) {
        const events = jtv.Result.withException(jtv.array(decodeStreamEvent(template)).run(json));
        haveSeenEvents = true;
        emitter.emit('events', events);
      } else if (isRecordWith('heartbeat', json)) {
        // NOTE(MH): If we receive the first heartbeat before any events, then
        // it's very likely nothing in the ACS matches the query. We signal this
        // by sending an empty list of events. This never does harm.
        if (!haveSeenEvents) {
          haveSeenEvents = true;
          emitter.emit('events', []);
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
}
