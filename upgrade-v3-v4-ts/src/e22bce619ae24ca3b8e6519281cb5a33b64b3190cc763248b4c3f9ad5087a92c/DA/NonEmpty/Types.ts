// Generated from DA/NonEmpty/Types.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@daml/types';

export type NonEmpty<a> = {
  hd: a;
  tl: a[];
}
export const NonEmpty = <a>(a: daml.Serializable<a>): daml.Serializable<NonEmpty<a>> => ({
  decoder: () => jtv.object({
    hd: a.decoder(),
    tl: daml.List(a).decoder(),
  }),
})
