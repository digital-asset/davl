// Generated from DA/Next/Map.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@digitalasset/daml-json-types';

export type Map<k_a9OV, v_a9OW> = {
  textMap: { [key: string]: v_a9OW };
}
export const Map = <k_a9OV, v_a9OW>(k_a9OV: daml.Serializable<k_a9OV>, v_a9OW: daml.Serializable<v_a9OW>): daml.Serializable<Map<k_a9OV, v_a9OW>> => ({
  decoder: () => jtv.object({
    textMap: daml.TextMap(v_a9OW).decoder(),
  }),
})
