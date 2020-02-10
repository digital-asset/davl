import * as jtv from '@mojotech/json-type-validation'
import Ledger, { Query, Event, CreateEvent } from '@daml/ledger';
import { Party, Template, List, Text, ContractId } from '@daml/types';

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

interface EventEmitter<EventMap> {
  on<E extends keyof EventMap>(type: E, listener: (ev: EventMap[E]) => void): void;
  off<E extends keyof EventMap>(type: E, listener: (ev: EventMap[E]) => void): void;
  allOff<E extends keyof EventMap>(type?: E): void;
  dispatch<E extends keyof EventMap>(type: E, ev: EventMap[E]): void;
}

function EventEmitter<EventMap>(): EventEmitter<EventMap> {
  let listeners: { [E in keyof EventMap]?: ((ev: EventMap[E]) => void)[] } = {};

  const on = <E extends keyof EventMap>(type: E, listener: (ev: EventMap[E]) => void): void => {
    if (!(type in listeners)) {
      listeners[type] = []
    }
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    listeners[type]!.push(listener);
  }

  const off = <E extends keyof EventMap>(type: E, listener: (ev: EventMap[E]) => void): void => {
    if (type in listeners) {
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      const typeListeners = listeners[type]!;
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      const index = typeListeners.findIndex(l => l === listener);
      if (index >= 0) {
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
        typeListeners.splice(index, 1);
      }
    }
  }

  const allOff = <E extends keyof EventMap>(type?: E) => {
    if (type) {
      listeners[type] = undefined;
    } else {
      listeners = {};
    }
  }

  const dispatch = <E extends keyof EventMap>(type: E, ev: EventMap[E]): void => {
    if (type in listeners) {
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      for (const listener of listeners[type]!) {
        listener(ev);
      }
    }
  }

  return {on, off, allOff, dispatch};
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
      this.wsBaseUrl = `ws://${window.location.hostname}:7575/`;
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
    type EventMap = {
      events: readonly Event<T, K, I>[];
      close: EventStreamCloseEvent;
    }
    const emitter = EventEmitter<EventMap>();
    ws.onopen = () => {
      const payload = {templateIds: [template.templateId], query};
      ws.send(JSON.stringify(payload));
    };
    // NOTE(MH): We ignore the 'error' event since it is always followed by a
    // 'close' event, which we need to handle anyway.
    ws.onerror = null;
    ws.onmessage = event => {
      const json: unknown = JSON.parse(event.data);
      if (Array.isArray(json)) {
        const events = jtv.Result.withException(jtv.array(decodeStreamEvent(template)).run(json));
        haveSeenEvents = true;
        emitter.dispatch('events', events);
      } else if (isRecordWith('heartbeat', json)) {
        // NOTE(MH): If we receive the first heartbeat before any events, then
        // it's very likely nothing in the ACS matches the query. We signal this
        // by sending an empty list of events. This never does harm.
        if (!haveSeenEvents) {
          haveSeenEvents = true;
          emitter.dispatch('events', []);
        }
      } else if (isRecordWith('warnings', json)) {
        console.warn('Ledger.streamQuery warnings', json);
      } else if (isRecordWith('errors', json)) {
        console.error('Ledger.streamQuery errors', json);
      } else {
        console.error('Ledger.streamQuery unknown message', json);
      }
    };
    ws.onclose = ({code, reason}) => {
      emitter.dispatch('close', {code, reason});
    }
    const close = () => {
      emitter.allOff();
      ws.close();
    }
    const {on, off} = emitter;
    return {on, off, close};
  }
}
