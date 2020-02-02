// Generated from DA/Internal/Prelude.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@daml/types';

export type Down<aAZ> = {
  unpack: aAZ;
}
export const Down = <aAZ>(aAZ: daml.Serializable<aAZ>): daml.Serializable<Down<aAZ>> => ({
  decoder: () => jtv.object({
    unpack: aAZ.decoder(),
  }),
})

export type Optional<aB2> = 
  |  { tag: 'None'; value: {} }
  |  { tag: 'Some'; value: aB2 }
export const Optional = <aB2>(aB2: daml.Serializable<aB2>): daml.Serializable<Optional<aB2>> => ({
  decoder: () => jtv.oneOf<Optional<aB2>>(
    jtv.object({tag: jtv.constant('None'), value: jtv.lazy(() => daml.Unit.decoder())}),
    jtv.object({tag: jtv.constant('Some'), value: jtv.lazy(() => aB2.decoder())}),
  ),
});
