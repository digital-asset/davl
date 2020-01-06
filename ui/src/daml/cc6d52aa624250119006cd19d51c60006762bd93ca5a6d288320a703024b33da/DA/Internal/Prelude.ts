// Generated from DA/Internal/Prelude.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@digitalasset/daml-json-types';

export type Down<aAZ> = {
  unpack: aAZ;
};
export const Down = <aAZ>(aAZ: daml.Serializable<aAZ>): daml.Serializable<Down<aAZ>> => ({
  decoder: () => jtv.object({
    unpack: aAZ.decoder(),
  }),
});

export type Optional<aB2> = unknown;
export const Optional = <aB2>(aB2: daml.Serializable<aB2>): daml.Serializable<Optional<aB2>> => ({
  decoder: jtv.unknownJson,
});
