// Generated from GHC/Types.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@daml/types';

export enum Ordering {
  LT = 'LT',
  EQ = 'EQ',
  GT = 'GT',
}
daml.STATIC_IMPLEMENTS_SERIALIZABLE_CHECK<Ordering>(Ordering)
// eslint-disable-next-line @typescript-eslint/no-namespace
export namespace Ordering {
  export const decoder = () => jtv.oneOf<Ordering>(
    jtv.constant(Ordering.LT),
    jtv.constant(Ordering.EQ),
    jtv.constant(Ordering.GT),
  );
}

export type Proxy<a> = {
}
export const Proxy = <a>(a: daml.Serializable<a>): daml.Serializable<Proxy<a>> => ({
  decoder: () => jtv.object({
  }),
})
