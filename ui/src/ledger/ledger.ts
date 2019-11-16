import Credentials from './credentials';
import { Archive, Choice, Contract, ContractId, Party, Template, Query } from '@digitalasset/daml-json-types';
import { array, Result } from '@mojotech/json-type-validation';

type LedgerResponse = {
  status: number;
  result: unknown;
}

type LedgerError = {
  status: number;
  errors: string[];
}

/**
 * An object of type `Ledger` represents a handle to a DAML ledger.
 */
class Ledger {
  readonly party: Party;
  private readonly token: string;

  constructor(credentials: Credentials) {
    this.party = credentials.party;
    this.token = credentials.token;
  }

  /**
   * Internal function to submit a command to the JSON API.
   */
  private async submit(method: string, payload: unknown): Promise<unknown> {
    const httpResponse = await fetch(method, {
      body: JSON.stringify(payload),
      headers: {
        'Authorization': 'Bearer ' + this.token,
        'Content-type': 'application/json'
      },
      method: 'post',
    });
    const json = await httpResponse.json();
    if (!httpResponse.ok) {
      console.log(json);
      // TODO(MH): Validate.
      const ledgerError = json as LedgerError;
      throw ledgerError;
    }
    // TODO(MH): Validate.
    const ledgerResponse = json as LedgerResponse;
    return ledgerResponse.result;
  }

  /**
   * Retrieve all contracts for a given template which match a query. See
   * https://github.com/digital-asset/daml/blob/master/docs/source/json-api/search-query-language.rst
   * for a description of the query language.
   */
  async query<T>(template: Template<T>, query: Query<T>): Promise<Contract<T>[]> {
    const payload = {"%templates": [template.templateId]};
    Object.assign(payload, query);
    const json = await this.submit('contracts/search', payload);
    return Result.withException(array(Contract(template).decoder()).run(json));
  }

  /**
   * Retrieve all contracts for a given template.
   */
  async fetchAll<T>(template: Template<T>): Promise<Contract<T>[]> {
    return this.query(template, {} as Query<T>);
  }

  /**
   * Mimic DAML's `lookupByKey`. The `key` must be a formulation of the
   * contract key as a query.
   */
  async pseudoLookupByKey<T>(template: Template<T>, key: Query<T>): Promise<Contract<T> | undefined> {
    const contracts = await this.query(template, key);
    if (contracts.length > 1) {
      throw Error("pseudoLookupByKey: query returned multiple contracts");
    }
    return contracts[0];
  }

  /**
   * Mimic DAML's `fetchByKey`. The `key` must be a formulation of the
   * contract key as a query.
   */
  async pseudoFetchByKey<T>(template: Template<T>, key: Query<T>): Promise<Contract<T>> {
    const contract = await this.pseudoLookupByKey(template, key);
    if (contract === undefined) {
      throw Error("pseudoFetchByKey: query returned no contract");
    }
    return contract;
  }

  /**
   * Create a contract for a given template.
   */
  async create<T>(template: Template<T>, argument: T): Promise<Contract<T>> {
    const payload = {
      templateId: template.templateId,
      argument,
    }
    const json = await this.submit('command/create', payload);
    return Result.withException(Contract(template).decoder().run(json));
  }

  /**
   * Exercise a choice on a contract.
   */
  async exercise<T, C>(choice: Choice<T, C>, contractId: ContractId<T>, argument: C): Promise<unknown> {
    const payload = {
      templateId: choice.template.templateId,
      contractId,
      choice: choice.choiceName,
      argument,
    };
    const json = await this.submit('command/exercise', payload);
    return json;
  }

  /**
   * Mimic DAML's `exerciseByKey`. The `key` must be a formulation of the
   * contract key as a query.
   */
  async pseudoExerciseByKey<T, C>(choice: Choice<T, C>, key: Query<T>, argument: C): Promise<unknown> {
    const contract = await this.pseudoFetchByKey(choice.template, key);
    return this.exercise(choice, contract.contractId, argument);
  }

  /**
   * Archive a contract given by its contract id.
   */
  async archive<T>(template: Template<T>, contractId: ContractId<T>): Promise<unknown> {
    return this.exercise(Archive(template), contractId, {});
  }

  /**
   * Archive a contract given by its contract id.
   */
  async pseudoArchiveByKey<T>(template: Template<T>, key: Query<T>): Promise<unknown> {
    return this.pseudoExerciseByKey(Archive(template), key, {});
  }
}

export default Ledger;
