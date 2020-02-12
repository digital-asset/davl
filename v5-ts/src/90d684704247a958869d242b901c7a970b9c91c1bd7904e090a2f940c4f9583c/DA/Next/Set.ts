// Generated from DA/Next/Set.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@daml/types';

export type Set<a_aavf> = {
  textMap: { [key: string]: {} };
}
export const Set = <a_aavf>(a_aavf: daml.Serializable<a_aavf>): daml.Serializable<Set<a_aavf>> => ({
  decoder: () => jtv.object({
    textMap: daml.TextMap(daml.Unit).decoder(),
  }),
})
