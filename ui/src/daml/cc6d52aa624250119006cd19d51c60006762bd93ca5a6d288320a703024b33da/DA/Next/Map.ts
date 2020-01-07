// Generated from DA/Next/Map.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@digitalasset/daml-json-types';

export type Map<a8Om, a8On> = {
  textMap: { [key: string]: a8On };
};
export const Map = <a8Om, a8On>(a8Om: daml.Serializable<a8Om>, a8On: daml.Serializable<a8On>): daml.Serializable<Map<a8Om, a8On>> => ({
  decoder: () => jtv.object({
    textMap: daml.TextMap(a8On).decoder(),
  }),
});
