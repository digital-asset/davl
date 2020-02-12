// Generated from DA/Internal/Down.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@daml/types';

export type Down<a> = {
  unpack: a;
}
export const Down = <a>(a: daml.Serializable<a>): daml.Serializable<Down<a>> => ({
  decoder: () => jtv.object({
    unpack: a.decoder(),
  }),
})
