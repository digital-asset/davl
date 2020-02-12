// Generated from GHC/Tuple.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@daml/types';

export type Unit<a> = {
  _1: a;
}
export const Unit = <a>(a: daml.Serializable<a>): daml.Serializable<Unit<a>> => ({
  decoder: () => jtv.object({
    _1: a.decoder(),
  }),
})
