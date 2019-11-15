// Generated from GHC/Types.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-unused-vars */
import * as daml from '../../../ledger/types';
import * as jtv from '@mojotech/json-type-validation';
import packageId from '../packageId';

const moduleName = 'GHC.Types';
const templateId = (entityName: string): daml.TemplateId => ({packageId, moduleName, entityName});

export type Ordering = unknown;
export const Ordering: daml.Serializable<Ordering> = ({
  decoder: jtv.unknownJson,
});

export type Proxy<a> = {
};
export const Proxy = <a>(a: daml.Serializable<a>): daml.Serializable<Proxy<a>> => ({
  decoder: () => jtv.object({
  }),
});
