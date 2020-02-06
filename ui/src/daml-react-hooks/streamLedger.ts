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
    onEvents: (events: Event<T, K, I>[]) => void,
    query?: Query<T>,
  ): () => void {
    // TODO(MH): When this moves into the proper `Ledger` class, we should use
    // `this.token` instead of this hack around privacy using `this['token']`.
    const protocols = ['jwt.token.' + this['token'], 'daml.ws.auth'];
    const ws = new WebSocket(this.wsBaseUrl + 'contracts/searchForever', protocols);
    let haveSeenEvents = false;
    ws.onerror = event => {
      console.error('/contracts/searchForever error', event);
      throw Error('/contracts/searchForever error');
    };
    ws.onopen = event => {
      const payload = {templateIds: [template.templateId], query};
      ws.send(JSON.stringify(payload));
    };
    ws.onclose = event => {
      console.error('/contracts/searchForever closed', event);
      throw Error('/contracts/searchForever closed');
    };
    ws.onmessage = event => {
      const json: unknown = JSON.parse(event.data);
      if (Array.isArray(json)) {
        haveSeenEvents = true;
        onEvents(jtv.Result.withException(jtv.array(decodeStreamEvent(template)).run(json)));
      } else if (isRecordWith('heartbeat', json)) {
        // NOTE(MH): If we receive the first heartbeat before any events, then
        // it's very likely nothing in the ACS matches the query. We signal this
        // by sending an empty list of events. This never does harm.
        if (!haveSeenEvents) {
          haveSeenEvents = true;
          onEvents([]);
        }
      } else if (isRecordWith('warnings', json)) {
        console.warn('/contracts/searchForever: warnings', json.warnings);
      } else if (isRecordWith('errors', json)) {
        console.error('/contracts/searchForever: errors', json.errors);
      } else {
        console.error('/contracts/searchForever: unknown message', json);
      }
    };
    return () => ws.close();
  }
}
