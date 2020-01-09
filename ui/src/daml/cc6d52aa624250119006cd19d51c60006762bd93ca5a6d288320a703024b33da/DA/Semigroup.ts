// Generated from DA/Semigroup.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@digitalasset/daml-json-types';

export type Max<a4ip> = {
  unpack: a4ip;
}
export const Max = <a4ip>(a4ip: daml.Serializable<a4ip>): daml.Serializable<Max<a4ip>> => ({
  decoder: () => jtv.object({
    unpack: a4ip.decoder(),
  }),
});

export type Min<a4iq> = {
  unpack: a4iq;
}
export const Min = <a4iq>(a4iq: daml.Serializable<a4iq>): daml.Serializable<Min<a4iq>> => ({
  decoder: () => jtv.object({
    unpack: a4iq.decoder(),
  }),
});
