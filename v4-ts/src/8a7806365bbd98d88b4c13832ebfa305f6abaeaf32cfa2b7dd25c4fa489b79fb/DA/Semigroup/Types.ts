// Generated from DA/Semigroup/Types.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@daml/types';

export type Min<a> = {
  unpack: a;
}
export const Min = <a>(a: daml.Serializable<a>): daml.Serializable<Min<a>> => ({
  decoder: () => jtv.object({
    unpack: a.decoder(),
  }),
})

export type Max<a> = {
  unpack: a;
}
export const Max = <a>(a: daml.Serializable<a>): daml.Serializable<Max<a>> => ({
  decoder: () => jtv.object({
    unpack: a.decoder(),
  }),
})
