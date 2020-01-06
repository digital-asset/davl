// Generated from DA/Next/Set.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@digitalasset/daml-json-types';

export type Set<a9z2> = {
  textMap: { [key: string]: {} };
};
export const Set = <a9z2>(a9z2: daml.Serializable<a9z2>): daml.Serializable<Set<a9z2>> => ({
  decoder: () => jtv.object({
    textMap: daml.TextMap(daml.Unit).decoder(),
  }),
});
