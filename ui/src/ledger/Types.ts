import { Decoder, object, string, optional, unknownJson, array } from '@mojotech/json-type-validation';

export interface Serializable<T> {
  // NOTE(MH): We need this to be a function to allow for mutually
  // recursive decoders.
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
}

/**
 * An interface for choice types. This is the counterpart of DAML's
 * `Choice` type class in DAML.
 */
export interface Choice<T, C> extends Serializable<C> {
  template: Template<T>;
  choiceName: string;
}

export const Archive = <T>(template: Template<T>): Choice<T, {}> => ({
  template,
  choiceName: 'Archive',
  decoder: () => object({}),
});

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

export const ContractId = <T extends unknown>(templateType: Template<T>): Serializable<ContractId<T>> => ({
  decoder: string,
});

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
  agreementText: Text;
  key: unknown;
  argument: T;
  witnessParties: Party[];
  workflowId?: string;
}

/**
 * Create a `Contract<T>` from its JSON representation. This is intended
 * for use by the `Ledger` class only.
 */
export const Contract = <T extends unknown>(templateType: Template<T>): Serializable<Contract<T>> => ({
  decoder: () => object({
    templateId: TemplateId.decoder(),
    contractId: ContractId(templateType).decoder(),
    signatories: array(party()),
    observers: array(party()),
    agreementText: text(),
    key: unknownJson(),
    argument: templateType.decoder(),
    witnessParties: array(party()),
    workflowId: optional(string()),
  }),
});
