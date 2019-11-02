import { Any, JsonObject, JsonProperty, JsonConvert, ValueCheckingMode, OperationMode } from "json2typescript";

/**
 * Identifier of a DAML template.
 */
@JsonObject("TemplateId")
export class TemplateId {
  @JsonProperty("packageId", String)
  packageId?: string = '';
  @JsonProperty("moduleName", String)
  moduleName: string = '';
  @JsonProperty("entityName", String)
  entityName: string = '';
}

/**
 * An interface for template types. This is the counterpart of DAML's
 * `Template` type class.
 */
export interface Template<T> {
  templateId: TemplateId;
  fromJSON(json: unknown): T;
  toJSON(t: T): unknown;
}

/**
 * An interface for choice types. This is the counterpart of DAML's
 * `Choice` type class in DAML.
 */
export interface Choice<T, C> {
  template: Template<T>;
  choiceName: string;
  toJSON(c: C): unknown;
}

export const Archive = <T>(template: Template<T>): Choice<T, {}> => {
  return {
    template,
    choiceName: 'Archive',
    toJSON: (_: {}): {} => {
      return {};
    },
  };
}

/**
 * The counterpart of DAML's `Party` type.
 */
export type Party = string;
export const Party = String;

export type AnyContractId = string;
export const AnyContractId = String;

/**
 * The counterpart of DAML's `ContractId T` type.
 */
export class ContractId<T> {
  readonly contractId: AnyContractId;

  constructor(contractId: AnyContractId) {
    this.contractId = contractId;
  }

  /**
   * Create a `ContractId<T>` from its JSON representation. This is intended
   * for use by the `Ledger` class only.
   */
  static fromJSON<T>(contractId: AnyContractId): ContractId<T> {
    return new ContractId<T>(contractId);
  }
  /**
   * Convert a `ContractId<T>` into its JSON representation. This is intended
   * for use by the `Ledger` class only.
   */

  toJSON = (): AnyContractId => this.contractId;
}

@JsonObject("AnyContract")
class AnyContract {
  @JsonProperty("observers", [Party])
  observers: Party[] = [];
  @JsonProperty("agreementText", String)
  agreementText: string = '';
  @JsonProperty("signatories", [Party])
  signatories: Party[] = [];
  @JsonProperty("key", Any, true)
  key: unknown = undefined;
  @JsonProperty("contractId", AnyContractId)
  contractId: AnyContractId = '';
  @JsonProperty("templateId", TemplateId)
  templateId: TemplateId = new TemplateId();
  @JsonProperty("witnessParties", [Party])
  witnessParties: Party[] = [];
  @JsonProperty("argument")
  argument: unknown = undefined;
  @JsonProperty("workflowId", String, true)
  workflowId?: string = '';
}

/**
 * A class representing a contract instance of a template type `T`. Besides
 * the contract data it also contains meta data like the contract id,
 * signatories, etc.
 */
export class Contract<T> {
  templateId: TemplateId = new TemplateId();
  contractId: ContractId<T>;
  signatories: Party[] = [];
  observers: Party[] = [];
  agreementText: string = '';
  key: unknown = undefined;
  data: T;
  witnessParties: Party[] = [];
  workflowId?: string = undefined;

  private constructor(contractId: ContractId<T>, argument: T) {
    this.contractId = contractId;
    this.data = argument;
  }

  /**
   * Create a `Contract<T>` from its JSON representation. This is intended
   * for use by the `Ledger` class only.
   */
  static fromJSON<T>(templateType: Template<T>, json: unknown): Contract<T> {
    const jsonConvert = new JsonConvert();
    jsonConvert.valueCheckingMode = ValueCheckingMode.DISALLOW_NULL;
    jsonConvert.operationMode = OperationMode.ENABLE;
    const anyContract = jsonConvert.deserializeObject(json, AnyContract);
    const contractId = ContractId.fromJSON<T>(anyContract.contractId);
    const argument = templateType.fromJSON(anyContract.argument);
    const contract = new Contract<T>(contractId, argument);
    contract.observers = anyContract.observers
    contract.agreementText = anyContract.agreementText
    contract.signatories = anyContract.signatories
    contract.key = anyContract.key
    contract.templateId = anyContract.templateId
    contract.witnessParties = anyContract.witnessParties
    contract.workflowId = anyContract.workflowId
    return contract;
  }
}
