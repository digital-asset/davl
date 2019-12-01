// Generated from DA/Next/Map.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as daml from '@digitalasset/daml-json-types';
import * as jtv from '@mojotech/json-type-validation';
import packageId from '../../packageId';

const moduleName = 'DA.Next.Map';
const templateId = (entityName: string): daml.TemplateId => ({packageId, moduleName, entityName});

export type Map<a8Om, a8On> = {
  textMap: { [key: string]: a8On };
};
export const Map = <a8Om, a8On>(a8Om: daml.Serializable<a8Om>, a8On: daml.Serializable<a8On>): daml.Serializable<Map<a8Om, a8On>> => ({
  decoder: () => jtv.object({
    textMap: daml.TextMap(a8On).decoder(),
  }),
});
