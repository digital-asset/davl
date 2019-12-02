// Generated from GHC/Types.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as daml from '@digitalasset/daml-json-types';
import * as jtv from '@mojotech/json-type-validation';

export type Ordering = unknown;
export const Ordering: daml.Serializable<Ordering> = ({
  decoder: jtv.unknownJson,
});

export type Proxy<a> = {
};
export const Proxy = <a>(a: daml.Serializable<a>): daml.Serializable<Proxy<a>> => ({
  decoder: () => jtv.object({
  }),
});
