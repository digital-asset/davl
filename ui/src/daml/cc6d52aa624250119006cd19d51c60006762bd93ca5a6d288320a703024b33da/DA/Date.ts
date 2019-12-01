// Generated from DA/Date.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as daml from '@digitalasset/daml-json-types';
import * as jtv from '@mojotech/json-type-validation';
import packageId from '../packageId';

const moduleName = 'DA.Date';
const templateId = (entityName: string): daml.TemplateId => ({packageId, moduleName, entityName});

export type Month = unknown;
export const Month: daml.Serializable<Month> = ({
  decoder: jtv.unknownJson,
});

export type DayOfWeek = unknown;
export const DayOfWeek: daml.Serializable<DayOfWeek> = ({
  decoder: jtv.unknownJson,
});
