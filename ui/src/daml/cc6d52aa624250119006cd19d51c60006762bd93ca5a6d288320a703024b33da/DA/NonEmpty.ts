// Generated from DA/NonEmpty.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as daml from '@digitalasset/daml-json-types';
import * as jtv from '@mojotech/json-type-validation';

export type NonEmpty<a8wG> = {
  hd: a8wG;
  tl: a8wG[];
};
export const NonEmpty = <a8wG>(a8wG: daml.Serializable<a8wG>): daml.Serializable<NonEmpty<a8wG>> => ({
  decoder: () => jtv.object({
    hd: a8wG.decoder(),
    tl: daml.List(a8wG).decoder(),
  }),
});
