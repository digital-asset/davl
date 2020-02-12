// Generated from DA/Next/Map.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@daml/types';

export type Map<k_a9Qi, v_a9Qj> = {
  textMap: { [key: string]: v_a9Qj };
}
export const Map = <k_a9Qi, v_a9Qj>(k_a9Qi: daml.Serializable<k_a9Qi>, v_a9Qj: daml.Serializable<v_a9Qj>): daml.Serializable<Map<k_a9Qi, v_a9Qj>> => ({
  decoder: () => jtv.object({
    textMap: daml.TextMap(v_a9Qj).decoder(),
  }),
})
