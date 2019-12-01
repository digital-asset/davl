// Generated from DA/Random.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as daml from '@digitalasset/daml-json-types';
import * as jtv from '@mojotech/json-type-validation';
import packageId from '../packageId';

const moduleName = 'DA.Random';
const templateId = (entityName: string): daml.TemplateId => ({packageId, moduleName, entityName});

export type Minstd = unknown;
export const Minstd: daml.Serializable<Minstd> = ({
  decoder: jtv.unknownJson,
});
