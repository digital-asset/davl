// Generated from DA/Next/Set.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@daml/types';

export type Set<a_aatS> = {
  textMap: { [key: string]: {} };
}
export const Set = <a_aatS>(a_aatS: daml.Serializable<a_aatS>): daml.Serializable<Set<a_aatS>> => ({
  decoder: () => jtv.object({
    textMap: daml.TextMap(daml.Unit).decoder(),
  }),
})
