// Generated from DA/Logic.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as daml from '@digitalasset/daml-json-types';
import * as jtv from '@mojotech/json-type-validation';
import packageId from '../packageId';

const moduleName = 'DA.Logic';
const templateId = (entityName: string): daml.TemplateId => ({packageId, moduleName, entityName});

export type Formula<a6PY> = unknown;
export const Formula = <a6PY>(a6PY: daml.Serializable<a6PY>): daml.Serializable<Formula<a6PY>> => ({
  decoder: jtv.unknownJson,
});
