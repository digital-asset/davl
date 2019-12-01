// Generated from DA/Time.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as daml from '@digitalasset/daml-json-types';
import * as jtv from '@mojotech/json-type-validation';
import packageId from '../packageId';

const moduleName = 'DA.Time';
const templateId = (entityName: string): daml.TemplateId => ({packageId, moduleName, entityName});

export type RelTime = {
  microseconds: daml.Int;
};
export const RelTime: daml.Serializable<RelTime> = ({
  decoder: () => jtv.object({
    microseconds: daml.Int.decoder(),
  }),
});
