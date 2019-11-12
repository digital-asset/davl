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
export interface Template<T extends {}> extends Serializable<T> {
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
 * The counterpart of DAML's `Party` type. We represent `Party`s as strings
 * representing valid party identifiers.
 */
export type Party = string;
export const Party: Serializable<Party> = {
  decoder: string,
}

/**
 * The counterpart of DAML's `Text` type. We represent `Text`s as `string`s.
 */
export type Text = string;
export const Text: Serializable<Text> = {
  decoder: string,
}

/**
 * The counterpart of DAML's `Int` type. We use strings to represent `Int`s
 * in order to avoid a loss of precision.
 */
export type Int = string;
export const Int: Serializable<Int> = {
  decoder: string,
}

/**
 * The counterpart of DAML's `Date` type. We represent `Date`s as strings of
 * the format "YYYY-MM-DD".
 */
export type Date = string;
export const Date: Serializable<Date> = {
  decoder: string,
}

/**
 * The counterpart of DAML's `ContractId T` type. We represent `ContractId`s
 * as strings.
 */
export type ContractId<T> = string;
export const ContractId = <T>(t: Serializable<T>): Serializable<ContractId<T>> => ({
  decoder: string,
});

/**
 * The counterpart of DAML's `Optional T` type.
 */
export type Optional<T> = T | unknown;
export const Optional = <T>(t: Serializable<T>): Serializable<Optional<T>> => ({
  decoder: () => optional(t.decoder()),
})

/**
 * The counterpart of DAML's `[T]` list type.
 */
export type List<T> = T[];
export const List = <T>(t: Serializable<T>): Serializable<T[]> => ({
  decoder: () => array(t.decoder()),
})

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
export const Contract = <T extends {}>(templateType: Template<T>): Serializable<Contract<T>> => ({
  decoder: () => object({
    templateId: TemplateId.decoder(),
    contractId: ContractId(templateType).decoder(),
    signatories: array(Party.decoder()),
    observers: array(Party.decoder()),
    agreementText: Text.decoder(),
    key: unknownJson(),
    argument: templateType.decoder(),
    witnessParties: array(Party.decoder()),
    workflowId: optional(string()),
  }),
});

export type Query<T> = T extends object ? {[K in keyof T]?: Query<T[K]>} : T;
