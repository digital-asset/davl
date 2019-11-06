import { Decoder, object, string, optional, unknownJson, array } from '@mojotech/json-type-validation';

export interface Serializable<T> {
  decoder: () => Decoder<T>;
}

/**
 * Identifier of a DAML template.
 */
export type TemplateId = {
  packageId?: string;
  moduleName: string;
  entityName: string;
}

const TemplateId = {
  decoder: (): Decoder<TemplateId> => object({
    packageId: optional(string()),
    moduleName: string(),
    entityName: string(),
  })
}

/**
 * An interface for template types. This is the counterpart of DAML's
 * `Template` type class.
 */
export interface Template<T> extends Serializable<T> {
  templateId: TemplateId;
  fromJSON: (json: unknown) => T;
  toJSON: (t: T) => unknown;
}

/**
 * An interface for choice types. This is the counterpart of DAML's
 * `Choice` type class in DAML.
 */
export interface Choice<T, C> extends Serializable<C> {
  template: Template<T>;
  choiceName: string;
  toJSON: (c: C) => unknown;
}

export const Archive = <T>(template: Template<T>): Choice<T, {}> => {
  return {
    template,
    choiceName: 'Archive',
    toJSON: (x: {}): {} => x,
    decoder: () => object({}),
  };
}

/**
 * The counterpart of DAML's `Party` type.
 */
export type Party = string;
export const party: () => Decoder<Party> = string;

export type Text = string;
export const text: () => Decoder<Text> = string;

export type Int = string;
export const int: () => Decoder<Int> = string;

export type Date = string;
export const date: () => Decoder<Date> = string;

/**
 * The counterpart of DAML's `ContractId T` type.
 */
export type ContractId<T> = string;

export const ContractId = {
  /**
   * Create a `ContractId<T>` from its JSON representation. This is intended
   * for use by the `Ledger` class only.
   */
  fromJSON: <T extends {}>(json: unknown): ContractId<T> => json as ContractId<T>,

  /**
   * Convert a `ContractId<T>` into its JSON representation. This is intended
   * for use by the `Ledger` class only.
   */
  toJSON: <T extends {}>(contractId: ContractId<T>): unknown => contractId,

  decoder: <T extends unknown>(): Decoder<ContractId<T>> => string(),
}

/**
 * A class representing a contract instance of a template type `T`. Besides
 * the contract data it also contains meta data like the contract id,
 * signatories, etc.
 */
export type Contract<T> = {
  templateId: TemplateId;
  contractId: ContractId<T>;
  signatories: Party[];
  observers: Party[];
  agreementText: string;
  key: unknown;
  argument: T;
  witnessParties: Party[];
  workflowId?: string;
}

/**
 * Create a `Contract<T>` from its JSON representation. This is intended
 * for use by the `Ledger` class only.
 */
export const Contract = {
  fromJSON: <T extends {}>(templateType: Template<T>, json: unknown): Contract<T> => json as Contract<T>,
  decoder: <T extends unknown>(templateType: Template<T>): Decoder<Contract<T>> => object({
    templateId: TemplateId.decoder(),
    contractId: ContractId.decoder(),
    signatories: array(party()),
    observers: array(party()),
    agreementText: string(),
    key: unknownJson(),
    argument: templateType.decoder(),
    witnessParties: array(party()),
    workflowId: optional(string()),
  }),
}
