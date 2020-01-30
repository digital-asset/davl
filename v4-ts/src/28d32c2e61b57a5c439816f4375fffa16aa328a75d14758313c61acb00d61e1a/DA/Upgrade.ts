// Generated from DA/Upgrade.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@digitalasset/daml-json-types';

export type MetaEquiv<m1_a4Us, m2_a4Ut> = {
}
export const MetaEquiv = <m1_a4Us, m2_a4Ut>(m1_a4Us: daml.Serializable<m1_a4Us>, m2_a4Ut: daml.Serializable<m2_a4Ut>): daml.Serializable<MetaEquiv<m1_a4Us, m2_a4Ut>> => ({
  decoder: () => jtv.object({
  }),
})
