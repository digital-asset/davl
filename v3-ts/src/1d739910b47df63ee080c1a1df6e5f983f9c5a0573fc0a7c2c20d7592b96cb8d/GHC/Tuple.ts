// Generated from GHC/Tuple.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@daml/types';

export type Unit<a3PX> = {
  _1: a3PX;
}
export const Unit = <a3PX>(a3PX: daml.Serializable<a3PX>): daml.Serializable<Unit<a3PX>> => ({
  decoder: () => jtv.object({
    _1: a3PX.decoder(),
  }),
})
