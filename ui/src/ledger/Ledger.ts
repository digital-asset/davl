import { Any, JsonObject, JsonProperty, JsonConvert, ValueCheckingMode } from "json2typescript";
import Credentials from './Credentials';
import { Archive, Choice, Contract, ContractId, Party, Template } from './Types';


@JsonObject("Ledger.Response")
class LedgerResponse {
  @JsonProperty("status", Number)
  status: number = -1;
  @JsonProperty("result", Any)
  result: unknown = undefined;
}

@JsonObject("Ledger.Error")
class LedgerError {
  @JsonProperty("status", Number)
  status: number = -1;
  @JsonProperty("errors", [String])
  errors: string[] = [];
}

/**
 * An object of type `Ledger` represents a handle to a DAML ledger.
 */
class Ledger {
  private readonly credentials: Credentials;

  constructor(credentials: Credentials) {
    this.credentials = credentials;
  }

  /**
   * Party whose authentication is used.
   */
  party = (): Party => this.credentials.party;

  /**
   * Internal function to submit a command to the JSON API.
   */
  private async submit(method: string, payload: unknown): Promise<unknown> {
    const jsonConvert = new JsonConvert();
    jsonConvert.valueCheckingMode = ValueCheckingMode.DISALLOW_NULL;
    const httpResponse = await fetch(method, {
      body: JSON.stringify(payload),
      headers: {
        'Authorization': 'Bearer ' + this.credentials.token,
        'Content-type': 'application/json'
      },
      method: 'post',
    });
    const json = await httpResponse.json();
    if (!httpResponse.ok) {
      console.log(json);
      throw jsonConvert.deserialize(json, LedgerError);
    }
    const ledgerResponse = jsonConvert.deserializeObject(json, LedgerResponse);
    return ledgerResponse.result;
  }

  /**
   * Retrieve all contracts for a given template which match a query. See
   * https://github.com/digital-asset/daml/blob/master/docs/source/json-api/search-query-language.rst
   * for a description of the query language.
   */
  async query<T>(template: Template<T>, query: unknown): Promise<Contract<T>[]> {
    const payload = {"%templates": [template.templateId]};
    Object.assign(payload, query);
    const json = await this.submit('contracts/search', payload);
    if (!(json instanceof Array)) {
      throw new Error('query: submit did not return an array');
    }
    const contracts = json.map((jsonContract) => Contract.fromJSON<T>(template, jsonContract));
    return contracts;
  }

  /**
   * Retrieve all contracts for a given template.
   */
  async fetchAll<T>(template: Template<T>): Promise<Contract<T>[]> {
    return this.query(template, {});
  }

  /**
   * Mimic DAML's `lookupByKey`. The `key` must be a formulation of the
   * contract key as a query.
   */
  async pseudoLookupByKey<T>(template: Template<T>, key: unknown): Promise<Contract<T> | undefined> {
    const contracts = await this.query(template, key);
    if (contracts.length > 1) {
      throw new Error("pseudoLookupByKey: query returned multiple contracts");
    }
    return contracts[0];
  }

  /**
   * Mimic DAML's `fetchByKey`. The `key` must be a formulation of the
   * contract key as a query.
   */
  async pseudoFetchByKey<T>(template: Template<T>, key: unknown): Promise<Contract<T>> {
    const contract = await this.pseudoLookupByKey(template, key);
    if (contract === undefined) {
      throw new Error("pseudoFetchByKey: query returned no contract");
    }
    return contract;
  }

  /**
   * Create a contract for a given template.
   */
  async create<T>(template: Template<T>, argument: T): Promise<Contract<T>> {
    const payload = {
      templateId: template.templateId,
      argument: template.toJSON(argument),
    }
    const json = await this.submit('command/create', payload);
    const contract = Contract.fromJSON<T>(template, json);
    return contract;
  }

  /**
   * Exercise a choice on a contract.
   */
  async exercise<T, C>(choice: Choice<T, C>, contractId: ContractId<T>, argument: C): Promise<unknown> {
    const payload = {
      templateId: choice.template.templateId,
      contractId: contractId.toJSON(),
      choice: choice.choiceName,
      argument: choice.toJSON(argument),
    };
    const json = await this.submit('command/exercise', payload);
    return json;
  }

  /**
   * Mimic DAML's `exerciseByKey`. The `key` must be a formulation of the
   * contract key as a query.
   */
  async pseudoExerciseByKey<T, C>(choice: Choice<T, C>, key: unknown, argument: C): Promise<unknown> {
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
  async pseudoArchiveByKey<T>(template: Template<T>, key: unknown): Promise<unknown> {
    return this.pseudoExerciseByKey(Archive(template), key, {});
  }

  static Error = LedgerError;
}

export default Ledger;
