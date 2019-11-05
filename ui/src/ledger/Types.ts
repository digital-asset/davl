/**
 * Identifier of a DAML template.
 */
export type TemplateId = {
  packageId?: string;
  moduleName: string;
  entityName: string;
}

/**
 * An interface for template types. This is the counterpart of DAML's
 * `Template` type class.
 */
export interface Template<T> {
  templateId: TemplateId;
  fromJSON: (json: unknown) => T;
  toJSON: (t: T) => unknown;
}

/**
 * An interface for choice types. This is the counterpart of DAML's
 * `Choice` type class in DAML.
 */
export interface Choice<T, C> {
  template: Template<T>;
  choiceName: string;
  toJSON: (c: C) => unknown;
}

export const Archive = <T>(template: Template<T>): Choice<T, {}> => {
  return {
    template,
    choiceName: 'Archive',
    toJSON: (x: {}): {} => x,
  };
}

/**
 * The counterpart of DAML's `Party` type.
 */
export type Party = string;

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
}
